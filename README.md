# carlyle

## What is it
Caryle is a REST based framework built on top of Ningle (extreme speeeeeed). 

## Quick start

This is based on Ningle so: 

```lisp
(defparameter *app* (make-instance 'carlyle-app))

(clack:clackup *app* :port 44560)

```
Done.

## An example

```lisp
(def-no-auth-api%get *app* ("/v1/events/:event-id" params :jojo)
  (find-event event-id))

(defun %test-get-event-id (id)
  (test-get (format nil "v1/events/~A" (quri:url-encode id))))

(def-no-auth-api%get *app* ("/v1/events" params :jojo-list)
  (sort (alexandria:hash-table-values *events*)
        #'local-time:timestamp< :key #'start-date))
```


## Basics
Caryle functions differently depending on the type of request you make, there are a few 
macros for defining API's.

```lisp
defapi%no-json
defapi%raw 
def-auth-api%post-raw
def-auth-api%get
def-no-auth-api%get
def-auth-api%post
def-no-auth-api%post
def-auth-api%delete
```

You can change how the json body is parsed by changing the single argument function provided as the key :parser, this function should process the raw body from the request.

### Validation
API's that dont have a body like those defined which are get and delete requests do not 
include a CRC header, those with a json body include a CRC header, this is verified 
automatically on receipt. If it fails then a condition is signalled and this is `composed` and then returned to the sender, see
```js
{ "error": { "code": 400, "category": "BAD-REQUEST" }, "info": { "description": "Bearer token is missing for a request that requires authorization." }, "recover": { "high": "Append authorization header", "low": "Resend request." } }
```
### Authorization 
Auth apis like those defined with def-auth-api%.. require an Authorization header which contains a valid bearer token, this uses a method called `find-bearer-token` and accepts two arguments, the token extracted from within the header, and the value of the key :bearer-verifier-args provided to the defapi macro. This is so you can modify how you verify a token, you can build varying authentication mechanisms.

```lisp
(defmethod find-bearer ((token bearer-token) where)
  token)

(defmethod carlyle:find-bearer-token ((token string) args)
  (if args 
      (apply #'find-bearer token args)
      (find-bearer token :session)))

(defmethod carlyle:find-bearer-token ((token null) args)
  (error 'malformed-bearer-token))

```

Carlyle provides no default routes for handling authentication and the creation/destruction of authorization tokens, you have to handle this yourself, here is an example:
```lisp
(def-no-auth-api%post *app* ("/v1/login" params json :jojo)
  (safe-destructure-keys (|username| |password|)
      json
    (let ((user (find-user |username|)))
      (if (and user (valid-password-p user |password|))
          (new-bearer-token user)
          (error 'forbidden)))))
```
`safe-destructure-keys` is like destructure-bind except it makes sure that all the keys listed in the args list are non nil.


### Adding conditions
When you want to add a new condition you just choose a superclass listed in src/conditions/conditions.lisp and then create a subclass of this, set the description and then add a `compose-condition/recover` entry as listed in src/conditions/compose.lisp and then signal this condition within the body of your defapi.

```lisp
(define-condition not-found%event (carlyle:not-found)
  ((description :initform "Event not found")))

(defmethod carlyle:compose-condition/recover append ((con not-found%event) req &rest args)
  (declare (ignore args))
  `(("high" . "Correct your event id.")))

(define-condition not-found%product (carlyle:not-found)
  ((description :initform "Product not found")))

(defmethod carlyle:compose-condition/recover append ((con not-found%product) req &rest args)
  (declare (ignore args))
  `(("high" . "Correct your product id.")))
```

Remember that the compose-condition/recover method uses the method combination APPEND.

### Validating types
Within the url of the examples you can see in the example section there are keywords like
`:event-id` when Carlyle encounters a variable like that within the URL then it automatically url decodes this and produces a variable within the body of your api by that name, however it also verifies the type of the argument, this is done using a method called `verify-param` which takes a keyword and a val, the keyword is parsed by Ningle, this is a means of determining what type of argument is expected, you can see from the code below that verify-param :user-id checks if val is of type 't%user-id, and t%user-id is a string that starts with !. 

```lisp
(defmethod verify-param ((key (eql :user-id)) val)
  (typep val 't%user-id))
```
You have to specialize a method for `verify-param` on every keyword you want within URLs.

### Post processing the body
There is a default method called `post-process-body` within src/json/json-protocol.lisp, this method is executed after the execution of body and receives the result from the execution as its first argument, the keyword WAY is specified in the `defapi` call, this is how 
you want to create the json that you return to the requestee, by default there is the key :JOJO and :JOJO-LIST however they both do the same thing, `(jojo:to-json result)`. The 
parameters list that is parsed by Ningle is also returned, this would mean you can apply 
query parameters to the result after the execution of the body.

```lisp
(defmethod post-process-body (result (way (eql :jojo)) params)
  "Single object uses jojo to generate the JSON."
  (jojo:to-json result))
```
The :around method also appends the CRC header to your response headers.




## License

MIT

