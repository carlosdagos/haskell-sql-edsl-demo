README
======

This repo contains the material I used for a HaskellerZ meetup in Zurich.

You can can see information about that [here](www.meetup.com/HaskellerZ/events/230150627/).

## Composable, type-safe SQL query generation in Haskell

Libraries like [Opaleye](https://github.com/tomjaguarpaw/haskell-opaleye/) and [Haskell Relational Record](https://github.com/khibino/haskell-relational-record) make use of Haskell's type system to provide an abstract way of composing SQL queries that are guaranteed to be valid.

#### Simple database connections

In the material I'm using PostgreSQL.

A basic way of talking to a PostgreSQL database is using [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple). While really useful, this library makes us write repetitive queries. Furthermore, it's string-based, so when running our queries against the database to get results in some Haskell data type, we're not guaranteed upon compliation time that the software is sound from a type perspective. This is not the "Haskell way" of writing programs.

#### What does it mean to be _composable_?

Composability means taking small, well-tested queries, and restricting them, joining them, aggregating, counting, etc. When we start joining queries, we also touch on the subject of projecting them. Projection means discarding columns from our queries.

Through projection and means of composition we can achieve arbitrarily complex results. Both Opaleye and Haskell Relational Record ensure meaningful composition with the use of different techniques.

#### What does it mean to be _type-safe_?

In Haskell, we normally see declarations such as `data Foo a = Foo { _a :: a }`. What's on the left side of the `=` is the type, and on the right are the terms. Type information is useful at compile time, Haskell makes great use of type information to make sure that our programs are safe.

If we can somehow abstract our queries and give them type information, then we could use them throughout our program to make sure that our queries "make sense" from a type perspective. This means that when we compare fields, that those fields are of the same type; when we're joining, our joins are upon compatible fields, etc. Furthermore we want to make sure that when we run queries against our database, that those results have a meaningful representation as Haskell values.

Both Opaleye and Haskell Relational Record exploit the type system via different means to make sure, on compile time, that our queries "make sense".

### Author

Carlos D'Agostino <carlos.dagostino@gmail.com>
