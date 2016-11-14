# Event Research Demo
# @Author: Mazi

# The key object in event effect analysis is ETS, which is simply a TS object with a different name in order to distinguish it form the ordinary TS.
# ETS is the object which indicating the specific date and stockID which the event occurred.
# There are two ways to obtain ETS. One is pass the ETS by yourself. The other is to use the functions to obtain it from JY database.

# Obtain ETS from JY database.
ETS <- getets.unfroz()
ETS <- getets.low_F_NP()
ETS <- getets.investor_visit()
ETS <- getets.employee_plan()
ETS <- getets.

# Obtain the "Daily Abnormal Return" in the event research window.
# win1 represents the days before the event date, and win2 represents the days after the event date.
TSErr <- EE_getTSErr(ETS, win1 = 20, win2 = 60)

# Plot TSErr
EE_Plot(TSErr)
EE_splityear(TSErr)

# Table TSErr
EE_table(TSErr)