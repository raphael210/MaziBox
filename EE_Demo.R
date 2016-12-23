# Event Effect Demo
# @Author: Mazi

# ----- Part 0. Setting up local database -----
# There are a few data sheets that is downloaded from Wind database. These datasets will be organized and stored in the local database and be prepared for further use.
library(WindR)
w.start()
lcdb.build.EE_employeeplan()
lcdb.build.EE_LeaderStockAlter()
lcdb.build.EE_pool()
lcdb.build.EE_score()
lcdb.build.EE_score_sum()

# ----- Part I. Event dffect research -----
# The key object in event effect analysis is ETS, which is simply a TS object with a different name in order to distinguish it form the ordinary TS.
# ETS is the object which indicating the specific date and stockID which the event occurred.

# There are two ways to obtain ETS.
# One is pass the ETS by yourself.
# The other is to use the following wrapped up functions that will return the corresponding ETS set.
ETS <- ets.unfroz()
ETS <- ets.low_F_NP()
ETS <- ets.leaderbuy()
ETS <- ets.leadersell()
ETS <- ets.leaderbuy_largebuy()
ETS <- ets.leadersell_largesell()
ETS <- ets.employee_plan()

# Obtain the "Daily Abnormal Return" in the event research window.
# win1 represents the days before the event date, and win2 represents the days after the event date. Both arguments are positive int.
TSErr <- EE_GetTSErr(ETS, win1 = 20, win2 = 60)

# Table TSErr
# The following function will return the summary table which indicating the average daily abnormal return with respect to the entire ETS set.
EE_table(TSErr)

# Plot TSErr
EE_Plot(TSErr)
EE_splityear(TSErr)

# ----- Part II. Eventscore for the multifactor-model -----
# For any TS object, get event score for the TS.
# The events that are put into consideration is included in the data "etsvec".
TS2 <- getETSscore(TS)




