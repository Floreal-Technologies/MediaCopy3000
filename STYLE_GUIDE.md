# Style Guide

The purpose of this style guide is to:

* Express the æsthetics and the soul of the project's team
* Make contributions easier by making the shape and location of new code predictable

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

## Pull Requests (PRs)

### Ticket

When fixing a problem, you SHOULD open a ticket that describes the issue and
the spirit of the fix, if you know it in advance. This allows other participants
to give insights before you spend time implementing the fix. Multiple PRs :can
target a single ticket if it helps with readability.

### Commits hygiene

You SHOULD use [Scoped Commits] for your commits. Commits of a PR MUST be
squashed upon merging.

Your PR commit SHOULD mention the ticket that it refers to, and you MAY use a
forge-specific keyword to close it.

Should your work be sponsored, your are invited to mention it with the
`Sponsored-By: <company name>` trailer. This allows us to properly give
attribution to our sponsors.

[RFC 2119]: https://datatracker.ietf.org/doc/html/rfc2119
[Scoped Commits]: https://scopedcommits.com/
