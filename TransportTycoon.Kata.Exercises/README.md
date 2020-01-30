# Exercise 1

I went for a simple F# script with [expecto](https://github.com/haf/expecto) tests.

The basic gist of it is to model time in 1 hour intervals. Trucks use the driveOneHour function, ships use the sailOneHour function. The factory and port cargo queues are part of the overall state.

The script produces results consistent with the exercise expectations. I'm not too happy with the state of the code, especially the truck driving code but it seems to be working as expected so I'll leave it at that and move on to the next exercise.
