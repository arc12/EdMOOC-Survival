Various "Survival Analysis" bits for Coursera.

In the first instance, just looking at the pattern over time of "last access time".

All people who made at least 1 access are considered. The official start course date was January 28^th 2013.

Courses are 5 weeks in duration except Introduction to Philosophy, which is 7 weeks long.

Need to decide on an end date for right censoring. For example, at what point would a last access time be forcibly cut off.

**for testing**, set this cutoff to be March 16th. People access aiplan until 23rd so 16th seems like a fair point to separate those who *might* have returned after 23rd from those who had probably dropped out. i.e. we assert "not sure what would have happened" for people who accessed on/after 16th.

People who gain course_grades.achievement_level != 'none' are excluded since their last access does not signify drop-out.


Raw Queries
-----------
The queries run on the MySQL data export from coursera are as follows (for AI Planning):


Learners who did not have a statement of achievement and whose last access time was before the end of the time window used for analysis.
> SELECT c.ip_continent continent, c.ip_country country, u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from vpodata_aiplangen.users u
    JOIN vpodata_aiplangen.uoe_ip_country c ON u.anon_user_id = c.anon_user_id
    JOIN vpodata_aiplangen.course_grades cg ON u.anon_user_id = cg.anon_user_id
          where access_group_id = 4 AND cg.achievement_level = 'none' AND last_access_time >=  1359244800 AND last_access_time < 1364688000

Similar, but the learners whose last access time was (on or) after the end of the analysis window.
> SELECT c.ip_continent continent, c.ip_country country, u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from vpodata_aiplangen.users u
    JOIN vpodata_aiplangen.uoe_ip_country c ON u.anon_user_id = c.anon_user_id
    JOIN vpodata_aiplangen.course_grades cg ON u.anon_user_id = cg.anon_user_id
          where access_group_id = 4 AND cg.achievement_level = 'none' AND last_access_time >= 1364688000

Learners who gained a statement of achievement.
> SELECT  u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from vpodata_aiplangen.users u
                   JOIN vpodata_aiplangen.course_grades cg ON u.anon_user_id = cg.anon_user_id
                      where access_group_id = 4 AND cg.achievement_level != 'none'

An unrestricted listing.
> SELECT c.ip_continent continent, c.ip_country country, u.anon_user_id, TRUNCATE(last_access_time/86400,0) date_index from vpodata_aiplangen.users u
    JOIN vpodata_aiplangen.uoe_ip_country c ON u.anon_user_id = c.anon_user_id
    JOIN vpodata_aiplangen.course_grades cg ON u.anon_user_id = cg.anon_user_id
          where access_group_id = 4 AND last_access_time >  1356825600