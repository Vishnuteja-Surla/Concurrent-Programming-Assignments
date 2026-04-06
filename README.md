# CS 6868: Concurrent Programming

This repository contains the programming assignments and research mini projects for the course **CS 6868: Concurrent Programming** (Graduate Level – M.Tech/MS/PhD).

The goal of this repository is to provide clear, well-structured implementations and analyses of modern concurrent and parallel programming concepts, spanning synchronization mechanisms, memory models, structured concurrency, and safe parallel programming techniques.

---

## Course Learning Objectives

By completing the assignments in this repository, students will:

**Parallelism Foundations:** Understand synchronization primitives, memory consistency models, and contention management in shared-memory systems.

**Concurrency Abstractions:** Gain hands-on experience with continuations, effect handlers, concurrency monads, schedulers, and asynchronous I/O.

**Correctness & Safety:** Learn to reason formally about data races, mutual exclusion, and Data-Race-Free (DRF) parallel programming.

**Concurrent Data Structures:** Design and implement fine-grained and lock-based concurrent data structures.

**Performance Analysis:** Analyze trade-offs between blocking and non-blocking synchronization, scalability, and contention behavior.

---

## Course Content

Assignments and implementations are aligned with the official course outline, including:

### Parallelism

* Mutual Exclusion
* Concurrent Objects
* Relaxed Memory Models
* Spin Locks
* Contention and Scalability
* Blocking Synchronisation
* Fine-grained Concurrent Data Structures

### Concurrency

* Continuations
* Concurrency Monads
* Effect Handlers
* Schedulers
* Concurrent Data Structures
* Asynchronous I/O

### Safe Concurrent Programming

* Modes
* Data-Race-Free (DRF) Parallel Programming

Reference Material:

* Xavier Leroy, *Control Structures (2nd Edition)*
  [https://xavierleroy.org/control-structures/](https://xavierleroy.org/control-structures/)

> Note: The course content is exploratory in nature and is neither fully sound nor complete.

---

## Assignments Overview

### Assignment 1: Tree Lock

Design and implement a scalable mutual exclusion lock for $n$ threads using Peterson’s algorithm arranged in a binary tree.

**Emphasis:** synchronization design, lock ordering, scalability, and structural correctness.

---

### Assignment 2: Atomic Snapshot
Implement a lock-free atomic snapshot object using the double-collect algorithm and rigorously test it using QCheck-Lin, QCheck-STM, and ThreadSanitizer.

**Emphasis:** linearizability, property-based testing, data-race detection, and systematic verification.

---

### Assignment 3: Batch Bounded Blocking Queue

Implement a bounded blocking queue with batch atomic operations and strict FIFO fairness using mutexes and condition variables

**Emphasis:** synchronization using locks and condition variables, fairness, and head-of-line blocking


## Evaluation Scheme

* **In-class Quizzes (Best 5/6)** – 20%
* **Mid-term Examination** – 20%
* **End Semester Examination** – 20%
* **Programming Assignments (4)** – 24%
* **Research Mini Project (Group of 2)** – 16%

---

## Implementation Details

* **Language:** OCaml
* **Build System:** dune
* Assignment-specific build and execution instructions are included in individual folders.

---

## Prerequisites

* Strong programming fundamentals
* Basic knowledge of operating systems
* Familiarity with functional programming (OCaml recommended)
* Prior exposure to memory models or parallel systems is helpful but not mandatory

---

## Acknowledgements

This repository was created as part of the **CS 6868: Concurrent Programming** course offered at **IIT Madras**.

---