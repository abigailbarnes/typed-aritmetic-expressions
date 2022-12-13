# typed-aritmetic-expressions
Correction (Tues 10/18): Corrected computation rule for greater than (see below).

Corrections (Mon 10/17): I had omitted congruence rules from greater-than, and I mistakenly wrote the same computation rule for eq twice. These mistakes have been fixed and you will find the corrections in purple below. Sorry! -ams

Greetings, everyone. HW3 extends the language of HW2 with some new forms, and adds a type checking phase to the language. The new forms are logical not, pair introduction and elimination, greater than on naturals, and an equality tester which works on any two terms of the same type.

As you will see in the code framework below, the evaluator always runs until a normal form is reached, even when the program is not well-typed. You will notice that some of the administrative functions like scan and parse are already complete in the framework.

The concrete syntax of the new forms is this, where <term> stands in for another concrete term and is not to be taken literally:

(not <term>)
(pair <term> <term>)
(first <term>)
(second <term>)
(> <term> <term>)
(eq <term> <term>)
The evaluation and typing rules for the new forms are given below. The evaluation and typing rules for the existing forms are all as has been previously presented. Notably, you will find typing rules most of the basic HW2 forms in Figure 8-2 of TaPL.

You are allowed to use as much of your HW2 code in HW3 as you like, but please make sure it's your own code and not borrowed from anyone else. If there are any parts on HW2 that you didn't successfully get working, now is your chance to fix them. If your HW2 is completely working, you will have an easier time with HW3 than otherwise.

Chapter 8 of TaPL
