This is a personal archive for my Advent of Code solutions

I started doing AOC in 2020, i believe i heard about it from [Tsoding](https://www.youtube.com/@TsodingDaily)

I used AOC as a great opportunity to learn a new language, a new programming paradigm

*I started writing this note in 2023, i don't really remember all the detail for the previous years, so i'll just write the quick summary for those years*

## 2023
This year, i use prolog.

It's a fun language so far, the paradigm not that far from the functional paradigm. The only cons i feel is the inability to write "inline predicate". It does make sense i guess, it suppose to be relation predicate, not a function.
*But maybe there's a way to achieve similar thing syntactically and i just don't know it yet*

### Day 8
<!-- <details> -->
<!-- <summary> -->
<!-- read more: -->
<!-- </summary> -->
Day 8 is the first day i reach some "technical" issue. It's definitely faster to use thing like hashmap to lookup the network. My first attempt was to just do linear list search, I'm hoping that the intrepeter/compiler can understand the code and turn it into a hashmap, but it doesn't. After searching for a while, i found [Association List](https://www.swi-prolog.org/pldoc/man?section=assoc) that was part of SWI-prolog. Assoc uses AVL (i think it's worse than your good ol hashmap, but i guess that'll do for now). The code run a bit faster, from ~2.0s to ~0.7s.

For the second part, i tried to bruteforce it, but then i reached the stack limit 

> swi prolog Stack limit (1.0Gb) exceeded

This is a weird technical limit of using these kind of langauages, looping is usually done by using recursion, and it's hard to control the memory usage of the program. There's definitely variables that i want to `free` from the memory, but i'm not sure if prolog allows the user to do that. But then i tried to think of another solution

Looking at the example, there's an interesting pattern. Each "ghost" from each `start` node only visits one of the `exit` node, and it happened periodically. 
```
for each start : [11A,22A]
11A : [2-11Z, 4-11Z, 6-11Z, 8-11Z]
22A : [3-22Z, 6-22Z]
```
*read `11A : [...,4-11Z,...]` as the ghost that started from `11A` visits `11Z` at step `4`*

But there is no indication in the problem statement that it would be the case. So i try to look at my input case
```
for each start : [MLA,BQA,MJA,AAA,TGA,BJA]
MLA : [19241-KPZ,38482-KPZ,57723-KPZ,76964-KPZ]
BQA : [18157-BDZ,36314-BDZ,54471-BDZ,72628-BDZ]
MJA : [19783-GNZ,39566-GNZ,59349-GNZ,79132-GNZ]
AAA : [16531-ZZZ,33062-ZZZ,49593-ZZZ,66124-ZZZ]
TGA : [21409-RFZ,42818-RFZ,64227-RFZ]
BJA : [14363-TMZ,28726-TMZ,43089-TMZ,57452-TMZ,71815-TMZ]
```
And yeah, looking at it, the pattern does holds. Knowing this, the problem became so much simpler, just find out when is the first time each "ghost" visit an `exit`, and the find the LCM for all of the numbers.

The result turned out to be `24035773251517`. So even if i can free the memory, bruteforce was never going to solve this problem anyway.
<!-- </details> -->


## 2022
Before the start of AOC, i thought i'm going to use AOC 2022 to learn assembly. But after trying to write my first assembly code, i pretty much gave up on the idea. Assembly is far too simple of a "language". Sure maybe i can solve day 1 with it, but the entire AOC? No thanks. So instead, i continue using haskell.

This year, i solved all the problems (all part1 & part2) until day 18, i believe i got bored with it and started another project

*But i revisited the problems again in 2023. To make it quick, i use python and boy it was way easier. Maybe there's a real reason why functional programming isn't really that popular.*

## 2021
Second year, i believe at the start of AOC, i don't really know what language to use. And then, AOC started. Not knowing what to do, i just use haskell again, now with more familiarity with the language and the paradigm, but of course there's still a lot to learn.

This year, i solved all the problems (all part1 & part2) until day 22, and then i stopped. I think i have something else to do, maybe something university related, or maybe i'm just bored. It's just 3 more days, maybe i'll revisit and finish this later


## 2020
My First AOC, i use it to learn Haskell. I've never use Haskell before, this pure functional paradigm is still very new to me at that time. I kinda know what to expect with Haskell from watching [Tsoding](https://www.youtube.com/@TsodingDaily), but other than that, i know pretty much nothing about it.

Pure Functional Programming is a beast. It's so different from your standard imperative programming. At the end of the challenge, i became more familiar with haskell and the functional paradigm in general. I still remember that "changing" an element from a List is a hassle, especially for 2D List. But other than that, the logic is fun.

This year, i solved all of the part1 but i missed some of the part2 (not sure whether i'll revisit them again or not)
