# Dining philosophers problem

An implementation of the dining philosophers problem in Erlang.

## Correct and buggy versions

For each solution implemented, we include two versions of the code:
* Correct version: A correct solution.
* Buggy version: A solution that contains a bug.

The buggy versions have filenames of the form `*_bug.erl`

## Solutions

The implemented solutions always include an arbitrator process in order to avoid regular deadlocks. However, these solutions differ in the following:
* `dining`: The standard solution. It spawns one process for each philosopher and each fork (a truly concurrent solution).
* `dining_simple`: Equal to the standard solution when comparing the correct versions. However, the buggy versions are different and introduce a bug in different places (which is easier to spot in this version).
* `dining_dict`: Similar to the standard solution in both the correct and buggy versions, but forks state in stored in a dictionary. Thus, forks are not processes, just an attribute of the waiter process.
