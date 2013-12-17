Various "Survival Analysis" bits.

In the first instance, just looking at the pattern over time of "last access time".

All people who made at least 1 access are considered. The official start course date was January 28^th 2013.

Courses are 5 weeks in duration except Introduction to Philosophy, which is 7 weeks long.

Need to decide on an end date for right censoring. For example, at what point would a last access time be forcibly cut off.

**for testing**, set this cutoff to be March 16th. People access aiplan until 23rd so 16th seems like a fair point to separate those who *might* have returned after 23rd from those who had probably dropped out. i.e. we assert "not sure what would have happened" for people who accessed on/after 16th.

People who gain course_grades.achievement_level != 'none' are excluded since their last access does not signify drop-out.