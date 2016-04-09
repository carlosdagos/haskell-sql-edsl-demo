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
I'm Carlos.

I've been paid to write Java, Scala and PHP.

I've been kindly asked to write Ruby, Python, Clojure, Objective-C.

I'm in (newfound) love with Haskell.
]

---

## What this talk is about

#### Results oriented

We're concerned with results rather than performance.

#### Not about ORMs

Generating queries is not about mapping data types to database rows.

#### PostgreSQL

The commonly-supported RDMBS between the libraries we'll be seeing is
PostgreSQL. Another one would be sqlite.red[\*], which is a simpler database
engine.

.footnote[.red.bold[\*] Although to use sqlite we'd have to download an
alternative version of Opaleye.]

---

## Sample application and design

We'll be looking at how to make an application that will allow me
to add TODOs, read them, and complete or discard them.

#### Sample output

We want to see all our TODOs

```bash
$ todos list
1. Buy food             (due by: tomorrow   )  (priority: 5 )
2. Call parents         (due by: tomorrow   )  (priority: 7 )
3. Wash clothes         (due by: 23 May 2016)  (priority: - )
4. Finish presentation  (due by: today      )  (priority: 10)
5. Call boss            (due by: 18 May 2016)  (priority: 20)
```

We want to find specific ones

```bash
$ todos find 1
Todo:     Buy food
Due by:   Tomorrow
Priority: 5
Hashtags: #independence #responsible
```
---

## Sample application and design

#### Sample output

We want to complete a TODO

```bash
$ todos complete 4
👍🏼 Completed 'Finish presentation #haskellerz'
```

Of course we also want to add a TODO

```bash
$ todos add 'Display presentation at Haskellerz!' \
            --due-by   '19-05-2016'               \
            --hashtags '#haskellerz, #fun'        \
            --priority 10
Added TODO with id 6
```

So far this is a basic CR~~U~~D. But what about more complex queries? No software
is complete without its flags. Plus, from `$ todos find 1` we saw that our app is aware
of the concept of a "hashtag", as well as due dates.

---

## Sample application and design

#### Sample output

List TODOs that are due by a certain date.

```bash
$ todos list --due-by tomorrow --with-hashtags
1. Buy food     (due by: tomorrow) (priority: 5) #independence #responsible
2. Call parents (due by: tomorrow) (priority: 7) #good-son
```

List TODOs that are due on a certain date, and belong to a certain hashtag.

```bash
$ todos list --due-by tomorrow --order-by-priority --hashtag "responsible"
2. Call parents (due by: tomorrow) (priority: 7) #good-son
1. Buy food     (due by: tomorrow) (priority: 5) #independence #responsible
```

List TODOs that are already late...

```bash
$ todos list --late --with-hashtags
5. Call boss (due by: 18 May 2016) (priority: 20) #good-employee
```

---

## Sample application and design

#### Database design

.pull-left[
<br>
<img src="images/todo_db.png" alt="todo database design" width="340" />
]

.pull-right[
```sql
create table todos(
  id serial primary key,
  title varchar(50) not null,
  due_date date not null,
  prio int
)

create table hashtags(
  todo_id int not null,
  hashtag varchar(50) not null,
  primary key (todo_id, hashtag)
)
```
]

---

## Sample application and design

#### (Some of the) queries we'll need

To see all our TODOs
```sql
select * from todos
```
To find a specific TODO
```sql
select * from todos where id = ?
```
To complete a TODO
```sql
delete from todos where id = ?
```
To add a TODO
```sql
insert into todos (title, due_date, prority) values (?, ?, ?)
```
---

## Sample application and design

#### Database design

List TODOs that are due by a specific date
```sql
select * from todos where due_date = ?
```
List TODOs ordered by priority
```sql
select * from todos order by prio
```
List late TODOs
```sql
select * from todos where due_date < current_date
```
List TODOs due on a specific date and belonging to a certain hashtag
```sql
select * from hashtags h
join todos t on h.todo_id = t.id
where h.hashtag  = ?
and   t.due_date = ?
```

---

## Current state of things

#### Leon Smith's [`postgresql-simple`](http://hackage.haskell.org/package/postgresql-simple)

```haskell
type TodoId  = Int
newtype Prio = Maybe Int deriving (Show)

data Todo = Todo { getId       :: !TodoId
                 , getTitle    :: !String
                 , getDueDate  :: !String
                 , getPriority :: !Prio
                 } deriving (Show)

data Hashtag = Hashtag { getTodoId  :: !TodoId
                       , getHashtag :: String
                       } deriving (Show, Eq)

instance Eq Todo where
    x == y = getId x == getId y
```

---

## Haskell Relational Record (HRR)

---

## Opaleye

---

## Closing Notes & Considerations
