(lambda (env)
  ;; The content of the request can be found in stdin

  ;; Sack applications must return only once.
  
  ;; Output to stderr is considered as logging, both in this procedure
  ;; and the body producing procedure.
  
  ;; Behavior for output to stdout in this procedure is undefined.
  
  ;; The environment must be an immutable object, in the sense that if
  ;; a sack application is called twice with the same environment
  ;; object, it should behave as if the web server had gotten two
  ;; identical requests.

  ;; I'm trying to avoid any globals or dynamic environment or
  ;; anything like that. Everything relevant to the request and the
  ;; response should be in the environment and in the return value of
  ;; the sack application.
  
  ;; If there is a value that isn't in the env, it is as if it was #f.
  
  ;; Values that must be present:
  ;; The list '(0 1), representing this version of Sack.
  (env 'sack:version)
  ;; #t if the server expects (but does not guarantee!) that the
  ;; application will only be invoked this one time during the life of
  ;; its containing process. Normally, this will only be #t for a
  ;; server based on CGI (or something similar).
  (env 'sack:run-once)
  ;; #t if the application object may be simultaneously invoked by
  ;; another thread in the same process, #f otherwise.
  (env 'sack:multithread)
  ;; #t if an equivalent application object may be simultaneously
  ;; invoked by another process, #f otherwise.
  (env 'sack:multiprocess) ; bool, like rack

  ;; 'http or 'https, depending on the request URL.
  (env 'sack:url-scheme)
  ;; The server's hostname, DNS alias, or IP address as it would
  ;; appear in self-referencing URLs.
  (env 'sack:server-name)
  ;; The port number to which the request was sent.
  (env 'sack:server-port)
  ;; A virtual path to the sack application being executed, used for
  ;; self-referencing URLs.
  (env 'sack:script-name)
  ;; The extra path information, as given by the client. In other
  ;; words, sack apps can be accessed by their virtual pathname,
  ;; followed by extra information at the end of this path. The extra
  ;; information is sent as sack:path-info. This information should be
  ;; decoded by the server if it comes from a URL before it is passed
  ;; to the sack application.
  (env 'sack:path-info)
  ;; The information which follows the ? in the URL which referenced
  ;; this sack application. This information should be decoded by the
  ;; server if it comes from a URL before it is passed to the sack
  ;; application.
  (env 'sack:query-string) ; string, like rack
  
  ;; Lowercase string, ie "head". It's a string because we don't want
  ;; to need to know which ones that are accepted, which would be
  ;; required otherwise in order to prevent memory leak attacks.
  (env 'sack:request-method)
  ;; A closure taking a lowercase string as argument (eg
  ;; "content-type"), returning a list of the values for that header
  ;; (possibly '())
  ;;
  ;; It's a closure and not a hash or an a-list to make it possible to
  ;; use several different implementation techniques.
  ;;
  ;; This procedure should also be able to take a procedure as argument,
  ;; in which case it works as an iterator over all the headers.
  (env 'sack:headers)


  ;; Possible extensions: sack:session, sack:cookie

  ;; How to do them, if the environment must be immutable?

  ;; Calling the functions to mutate the environment after the sack
  ;; application has returned results in undefined behaviour.

  make-cookie
  cookie?
  cookie-name
  cookie-value
  cookie-expires
  cookie-domain
  cookie-path
  cookie-port
  cookie-secure
  cookie-http-only
  ((env 'sack:cookie:get-all)) ;; Returns a list of the cookies,
                               ;; including changes made by the app.
  ((env 'sack:cookie:get) name) ;; Returns a cookie or #f
  ((env 'sack:cookie:set!) cookie)

  ((env 'sack:session:get-all)) ;; Returns an a-list of the session data
  ((env 'sack:session:get) name)
  ((env 'sack:session:set!) name value) ;; Name must be a string
  ((env 'sack:session:delete!) name)
  ((env 'sack:session:destroy!))
  ((env 'sack:session:get-id)) ;; Possibly not there
  ((env 'sack:session:regenerate-id!)) ;; Possibly not there
  
  (values 200
          ;; If the headers don't include a Content-Length, and not a
          ;; Transfer-Encoding: chunked header, the content will be
          ;; chunk encoded.
          '(("Content-Type" . "text/html"))
          (lambda ()
            ;; Prints the body. This procedure will be called again
            ;; and again until it returns #f.
            )))
