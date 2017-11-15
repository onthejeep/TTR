library(RSQLite);

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation');
source('0_Utility.R')
source('0_DatabaseOperation.R');


Update.Grid = function()
{
    DestColIndex = 45;
    DestRowIndex = 47;

    SqliteName = sprintf('Result/ttr_to_(col_%02d_row_%02d).sqlite', DestColIndex, DestRowIndex);
    Connection2Sqlite = Database.Open.Sqlite(SqliteName);
    print('Database is open = %s', dbIsValid(Connection2Sqlite));

    TimeIndex.Set = c();
    DowName.Set = c('Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
    # 'Sunday', 'Monday', 

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
    unlink(SqliteName);
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
        # average travel time
        AvgTT = mean(PairsTraveltime[[i]]$Traveltime, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_avg] = %f where [id] = %d',
            dowName, TimeIndexNumber, AvgTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # median travel time
        MedianTT = median(PairsTraveltime[[i]]$Traveltime, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_avg] = %f where [id] = %d',
            dowName, TimeIndexNumber, MedianTT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # 90th percentile
        P90TT = quantile(PairsTraveltime[[i]]$Traveltime, 0.9, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_p90] = %f where [id] = %d',
            dowName, TimeIndexNumber, P90TT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # 95th percentile
        P95TT = quantile(PairsTraveltime[[i]]$Traveltime, 0.95, na.rm = T);
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_p95] = %f where [id] = %d',
            dowName, TimeIndexNumber, P95TT, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # buffer time
        BufferTime = (quantile(PairsTraveltime[[i]]$Traveltime, 0.90, na.rm = T) - AvgTT) / AvgTT;
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_bt] = %f where [id] = %d',
            dowName, TimeIndexNumber, BufferTime, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

        # planning time index
        PlanningTime = quantile(PairsTraveltime[[i]]$Traveltime, 0.95, na.rm = T) - AvgTT;
        UpdateCommand = sprintf('update [grid_kunshan_%s] set [ti_%d_pt] = %f where [id] = %d',
            dowName, TimeIndexNumber, PlanningTime, PairsTraveltime[[i]]$FID);
        Result = dbSendQuery(sqlite, UpdateCommand);
        dbClearResult(Result);

    }
    dbCommit(sqlite);
}


