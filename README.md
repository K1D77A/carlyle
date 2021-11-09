# carlyle

## What is it
Caryle is a REST based framework built on top of Ningle (extreme speeeeeed). 

## Basics
Caryle functions differently depending on the type of request you make, there are a few 
macros for defining API's.

```lisp
defapi%no-json
def-auth-api%get
def-no-auth-api%get
def-auth-api%post
def-no-auth-api%post
def-auth-api%delete
```
API's that dont have a body like those defined which are get and delete requests do not 
include a CRC header, those with a json body include a CRC header, this is verified 
automatically on receipt. If it fails then a condition is signalled and this is `composed` and then returned to the sender, see
```js
{ "error": { "code": 400, "category": "BAD-REQUEST" }, "info": { "description": "Bearer token is missing for a request that requires authorization." }, "recover": { "high": "Append authorization header", "low": "Resend request." } }
```

Auth apis like those defined with def-auth-api%.. require an Authorization header which contains a valid bearer token, this uses a method called `find-bearer-token` and accepts a single argument, the token extracted from within the header.

## License

MIT

