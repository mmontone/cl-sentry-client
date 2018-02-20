# cl-sentry-client

Sentry client for Common Lisp

WIP

## Basic usage

```lisp
(sentry-client:initialize-sentry-client <sentry-dsn>)
(with-sentry-error-handler () (error "test"))
```

## License

MIT
