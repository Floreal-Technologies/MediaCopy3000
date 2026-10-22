# Style Guide

_Stay stylish._

> The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
> NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
> "OPTIONAL" in this document are to be interpreted as described in
> [RFC 2119].

## Haskell

### Point-free style

You MUST NOT use point-free style that favours terseness over readability.
We read code when something bad happens to it, or we need a clear and easy
understand of what is happening.

- You MUST NOT use tuple sections. Write `\x -> (a, x)` instead of `(a,)`

- Three or more composed functions SHOULD be laid out as a pipeline with `(&)` and `(<&>)`, not `(.)`.

### Import qualification

You SHOULD use qualified imports for functions and open imports for types.

[RFC 2119]: https://datatracker.ietf.org/doc/html/rfc2119
