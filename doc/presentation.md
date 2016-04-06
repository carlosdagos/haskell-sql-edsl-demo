class: center, middle

# Generating SQL Queries in Haskell

#### Haskellerz Meetup
#### Zürich, May 19th 2016
.center[<img src="images/haskell.png" alt="" width="100" />]
<br/>
##### By [Carlos D.](https://github.com/charlydagos)

---

# Agenda

1. Introduction
2. What this talk is about
3. Current state of things
4. Sample application and design
5. Haskell Relational Record (HRR)
6. Opaleye
7. Closing notes. Considerations.

---

## Introduction

.left-column[
#### About me
]

.right-column[
I'm Carlos!

I've been paid to write Java, Scala and PHP.

I've been kindly asked to write Ruby, Python, Clojure, Objective-C.

I'm in (newfound) love with Haskell.
]

---

## What this talk is about

#### Results oriented

We're concerned with results rather than performance.

#### Not about ORMs

Generating queries is not about mapping data types to database
rows.

#### PostgreSQL

The commonly-supported RDMBS between the libraries we'll be seeing is
PostgreSQL. Another one would be sqlite.red[\*], which is a simpler database.

.footnote[.red.bold[\*] Although to use sqlite we'd have to download an
alternative version of Opaleye.]

---

## Sample application and design

---

## Current state of things

---

## Haskell Relational Record (HRR)

---

## Opaleye

---

## Closing Notes & Considerations
