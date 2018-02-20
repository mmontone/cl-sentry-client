# cl-sentry-client

Sentry client for Common Lisp

WIP

## Basic usage

```lisp
(sentry-client:initialize-sentry-client <sentry-dsn>)
(sentry-client:with-sentry-error-handler () (error "test"))
```

Or in your own error handler: 

```lisp
(handler-case (my-code-with-error)
   (error (e)
      (sentry-client:capture-exception e)
      ...)))
```

## License

MIT
