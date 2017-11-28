library(RSQLite);
library(outliers)


source('0_Utility.R')
source('0_DatabaseOperation.R');


# Result/tt_to_(col_45_row_47)_timeIndex_26_dow_Thursday.rdata
Update.Grid = function()
{
    DestColIndex = 45;
    DestRowIndex = 47;

    SqliteName = sprintf('Result/ttr_to_(col_%02d_row_%02d).sqlite', DestColIndex, DestRowIndex);
    Connection2Sqlite = Database.Open.Sqlite(SqliteName);
    print('Database is open = %s', dbIsValid(Connection2Sqlite));

    TimeIndex.Set = c();
    DowName.Set = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
    # 

    for (i in 0:23)
    {
        for (j in c('firsthalf', 'secondhalf'))
        {
            TimeIndex.Set = c(TimeIndex.Set, sprintf('%d:%s', i, j));
        }
    }

    for (i in DowName.Set)
    {
        print(sprintf('CurrentTime = %s; DOW = %s;', now(), i));
        for (j in TimeIndex.Set)
        {
            Update.Fields(Connection2Sqlite, DestColIndex, DestRowIndex, timeIndex = j, dowName = i);
        }
    }

    dbDisconnect(Connection2Sqlite);
    #unlink(SqliteName);
}

Update.Fields = function(sqlite, destColIndex, destRowIndex, timeIndex = '7:firsthalf', dowName = 'Tuesday')
{
    TimeIndexNumber = Find.TimeIndex(timeIndex);
    FileName = sprintf('Result/tt_to_(col_%02d_row_%02d)_timeIndex_%d_dow_%s.rdata',
        destColIndex, destRowIndex, TimeIndexNumber, dowName);
    load(FileName);
    PairsTraveltime = Pairs;

    if (is.na(PairsTraveltime) || length(PairsTraveltime) == 0)
    {
        return(NA);
    }

    print(sprintf('Load %s with %d records', FileName, length(PairsTraveltime)));

    dbBegin(sqlite);
    for (i in 1:length(PairsTraveltime))
    {
        Traveltimes = Detect.Remove.Outlier(PairsTraveltime[[i]]$Traveltime);

        if (is.na(Traveltimes))
        {
            next;
        }

        # sample size
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_sz] = %d where [id] = %d',
            dowName, TimeIndexNumber, length(Traveltimes), PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # average travel time
        AvgTT = mean(Traveltimes, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_avg] = %f where [id] = %d',
            dowName, TimeIndexNumber, AvgTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # median travel time
        MedianTT = median(Traveltimes, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_median] = %f where [id] = %d',
            dowName, TimeIndexNumber, MedianTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # std travel time
        StdTT = sd(Traveltimes, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_std] = %f where [id] = %d',
            dowName, TimeIndexNumber, StdTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # cov travel time
        COVTT = StdTT / AvgTT;
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_cov] = %f where [id] = %d',
            dowName, TimeIndexNumber, COVTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # 90th percentile
        P90TT = quantile(Traveltimes, 0.9, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_p90] = %f where [id] = %d',
            dowName, TimeIndexNumber, P90TT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # 95th percentile
        P95TT = quantile(Traveltimes, 0.95, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_p95] = %f where [id] = %d',
            dowName, TimeIndexNumber, P95TT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # buffer time
        BufferTime = quantile(Traveltimes, 0.90, na.rm = T) - AvgTT;
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_bt] = %f where [id] = %d',
            dowName, TimeIndexNumber, BufferTime, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # planning time index
        PlanningTime = (quantile(Traveltimes, 0.95, na.rm = T) - AvgTT) / AvgTT;
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_pt] = %f where [id] = %d',
            dowName, TimeIndexNumber, PlanningTime, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

    }
    dbCommit(sqlite);
}


Detect.Remove.Outlier = function(x)
{
    if (length(x) <= 3)
    {
        return(x);
    }

    Temp = Detect.Remove.Max(x);
    if (is.na(Temp) == F)
    {
        Temp = Detect.Remove.Max(Temp);
        Temp = Detect.Remove.Max(Temp);

        Result = Detect.Remove.Min(Temp);
        return(Result);
    }
    else
    {
        return(NA);
    }
    
}

Detect.Remove.Max = function(x, pvalue = 0.0001)
{
    Result = chisq.out.test(x);

    if (is.nan(Result$p.value) == F)
    {
        if (Result$p.value >= pvalue)
        {
            return(x);
        }
        else
            {
            MaxIndex = which.max(x);
            print(sprintf('The max value is %0.2f', max(x)));
            return(x[-MaxIndex]);
        }
    }
    else
    {
        return(NA);
    }
    
}

Detect.Remove.Min = function(x, pvalue = 0.0001)
{
    Result = chisq.out.test(x, opposite = F);

    if (is.nan(Result$p.value) == F)
    {
        if (Result$p.value >= pvalue)
        {
            return(x);
        }
        else
            {
            MinIndex = which.min(x);
            print(sprintf('The min value is %0.2f', min(x)));
            return(x[-MinIndex]);
        }
    }
    else
    {
        return(NA);
    }
    
}

#a = c(-105, seq(from = 10, to = 20, length = 60), 1000);
#a = rep(1, 6);
#Detect.Remove.Outlier(a);

#load('Result/tt_to_(col_45_row_47)_timeIndex_26_dow_Thursday.rdata');
#PairsTraveltime = Pairs;

#for (i in 1:length(PairsTraveltime))
#{
    #print(PairsTraveltime[[i]]$Traveltime);
    #Traveltimes = Detect.Remove.Outlier(PairsTraveltime[[i]]$Traveltime);
#}
