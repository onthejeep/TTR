library(lubridate)
library(RODBC);
library(RSQLite);

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation');

Update.Grid = function(destColIndex = 45, destRowIndex = 47,
                       timeIndex = '7:firsthalf', dowName = 'Tuesday')
{
    # ttr_to_(col_45_row_47).sqlite
    SqliteName = sprintf('Result/ttr_to_(col_%02d_row_%02d).sqlite',
    destColIndex, destRowIndex);

    Connection2Sqlite = Database.Open.Sqlite(SqliteName);
    print(dbIsValid(Connection2Sqlite));
    Connection2SQL = Database.Open.SqlServer();

    print('Get travel times to the destination ... ');
    PairsTraveltime = Get.ALLPairTraveltime(Connection2SQL, destColIndex, destRowIndex,
    timeIndex, dowName, numSampleThreshold = 6);

    print(sprintf('Updating Sqlite %s with %d records', SqliteName, length(PairsTraveltime)));

    dbBegin(Connection2Sqlite);
    for (i in 1:length(PairsTraveltime))
    {
        AvgTT = mean(PairsTraveltime[[i]]$Traveltime, na.rm = T);
        UpdateCommand = sprintf('update [Grid_Kunshan] set [mean] = %f where [pk_uid] = %d',
        AvgTT, PairsTraveltime[[i]]$FID + 1);

        # print(UpdateCommand);

        Result = dbSendQuery(Connection2Sqlite, UpdateCommand);
        dbClearResult(Result);
    }
    dbCommit(Connection2Sqlite);
    
    dbDisconnect(Connection2Sqlite);
    close(Connection2SQL);
    unlink(SqliteName);
}

Get.ALLPairTraveltime = function(dbConnection, destColIndex, destRowIndex, timeIndex, dowName, numSampleThreshold)
{
    Pairs = list();

    HotOrigin = Find.HotOrigin(dbConnection, destColIndex, destRowIndex, timeIndex, dowName, numSampleThreshold);

    if (nrow(HotOrigin) == 0)
    {
        save(Pairs, file = sprintf('Result/tt_to_(col_%02d_row_%02d)_timeIndex_%d_dow_%s.rdata',
        destColIndex, destRowIndex, Find.TimeIndex(timeIndex), dowName));
        return(NA);
    }
    #print(sprintf('DestCol = %02d; DestRow = %02d; TimeIndex = %s; DOW = %s; NumRow = %03d',
    #destColIndex, destRowIndex, timeIndex, dowName, nrow(HotOrigin)));

    for (i in 1:nrow(HotOrigin))
    {
        OriginColIndex = HotOrigin[i, 'Loading.ColIndex'];
        OriginRowIndex = HotOrigin[i, 'Loading.RowIndex'];

        Traveltime = Get.SinglePairTraveltime(dbConnection, destColIndex, destRowIndex,
        OriginColIndex, OriginRowIndex, timeIndex, dowName);

        SinglePair = list();
        SinglePair$DOW = dowName;
        SinglePair$OriginColIndex = OriginColIndex;
        SinglePair$OriginRowIndex = OriginRowIndex;
        SinglePair$FID = Find.FID(OriginColIndex, OriginRowIndex);
        SinglePair$Traveltime = Traveltime;

        Pairs[[i]] = SinglePair;
    }

    save(Pairs, file = sprintf('Result/tt_to_(col_%02d_row_%02d)_timeIndex_%d_dow_%s.rdata',
    destColIndex, destRowIndex, Find.TimeIndex(timeIndex), dowName));
    return(Pairs);
}

Get.SinglePairTraveltime = function(dbConnection, destColIndex, destRowIndex,
                         originColIndex, originRowIndex,timeIndex, dowName)
{
    DowNumber = Find.DOW(dowName);
    TimeIndexNumber = Find.TimeIndex(timeIndex);
    SelectCommand = sprintf('
select Temp.[Loading.ColIndex], Temp.[Loading.RowIndex], Temp.TimeIndex, Temp.DOW, Temp.Traveltime
from
(
select DATEPART(HOUR, [Loading.Time])* 2 + CEILING(DATEPART(MINUTE, [Loading.Time]) / 30.0) as [TimeIndex], DATEPART(WEEKDAY, [Loading.Time]) as DOW, *
from [View_Col_%d_Row_%d]
where Traveltime > 0 
) as Temp
where Temp.TimeIndex =  %d and DOW =  %d
and Temp.[Loading.ColIndex] =  %d and Temp.[Loading.RowIndex] =  %d',
destColIndex, destRowIndex, TimeIndexNumber, DowNumber,
originColIndex, originRowIndex);

    Result = sqlQuery(dbConnection, SelectCommand);

    return(Result[, 'Traveltime']);
}


Find.HotOrigin = function(dbConnection, destColIndex, destRowIndex, timeIndex, dowName, numSampleThreshold)
{
    DowNumber = Find.DOW(dowName);
    TimeIndexNumber = Find.TimeIndex(timeIndex);
    SelectCommand = sprintf("
select Temp.[Loading.ColIndex], Temp.[Loading.RowIndex], count(*) as NumSample
from
(
    select DATEPART(HOUR, [Loading.Time]) * 2 + CEILING(DATEPART(MINUTE, [Loading.Time]) / 30.0) as[TimeIndex], DATEPART(WEEKDAY, [Loading.Time]) as DOW, *
    from[View_Col_%02d_Row_%02d]
    where Traveltime > 0
) as Temp
where Temp.TimeIndex = %d and DOW = %d
group by Temp.[Loading.ColIndex], Temp.[Loading.RowIndex]
having count( * ) >=  %d
order by NumSample desc ", 
destColIndex, destRowIndex, TimeIndexNumber, DowNumber, numSampleThreshold);

    Result = sqlQuery(dbConnection, SelectCommand);
    return(Result);
}

Find.HotDestination = function(sqlite)
{
    SelectCommand = sprintf("
select [UnLoading.ColIndex], [UnLoading.RowIndex], Number
from [hot_destination]
order by Number desc");

    Result = dbGetQuery(sqlite, SelectCommand);
    View(Result);
    dbClearResult(Result);
}

# The conversion follows the standard of the built-in function dataprt(WEEKDAY, [timestamp]) in SQL Server 
Find.DOW = function(dowName = 'Monday')
{
    DowNumber = NA;
    switch(dowName,
           'Sunday' = { DowNumber = 1 },
            'Monday' = { DowNumber = 2 },
            'Tuesday' = { DowNumber = 3 },
            'Wednesday' = { DowNumber = 4 },
            'Thursday' = { DowNumber = 5 },
            'Friday' = { DowNumber = 6 },
            'Saturday' = { DowNumber = 7 },
    );
    return(DowNumber);
}

Find.TimeIndex = function(time)
{
    StringSplit = strsplit(time, ':')[[1]];
    NumHour = as.numeric(StringSplit[1]);
    NumMinute = 1;
    switch(StringSplit[2],
           'firsthalf' = { NumMinute = 1},
           'secondhalf' = { NumMinute = 2 }
    );

    return(NumHour * 2 + NumMinute);
}

# colIndex and rowIndex ranges from [1, 100]
Find.FID = function(colIndex, rowIndex)
{
    FID = 100 * (colIndex - 1) + (rowIndex - 1);
    return(FID);
}

# Database Connection ----
Database.Open.Sqlite = function(sqliteName)
{
    # 'Config/hot_od.sqlite'
    Connection = dbConnect(RSQLite::SQLite(), dbname = sqliteName);
    return(Connection);
}

Database.Open.SqlServer = function()
{
    Connection = odbcConnect('TaxiData', uid = 'Shu', pwd = 'Sql.slu2012');
    return(Connection);
}

# UnitTest ----
UnitTest.Find.HotDestination = function()
{
    Connection = Database.Open.Sqlite('Config/hot_od.sqlite');
    Tables = dbListTables(Connection);
    print(Tables);
    Find.HotDestination(Connection);
    dbDisconnect(Connection);
}

UnitTest.Find.HotOrigin = function()
{
    Connection = Database.Open.SqlServer();
    Result = Find.HotOrigin(Connection, 45, 47, '3:secondhalf', 'Tuesday', 5);
    close(Connection);

    return(Result);
}

UnitTest.Get.Traveltime = function()
{
    Connection = Database.Open.SqlServer();
    Result = Get.SinglePairTraveltime(Connection, 45, 47, 65, 8, '3:secondhalf', 'Tuesday');
    if (is.null(nrow(Result)))
    {
        print(NA);
    }
    else
    {
        print(Result[, 'Traveltime']);
    }
    close(Connection);
}

UnitTest.Update.Grid = function()
{
    Update.Grid(destColIndex = 45, destRowIndex = 47,
                       timeIndex = '7:firsthalf', dowName = 'Tuesday');
}

UnitTest.Get.ALLPairTraveltime = function()
{
    TimeIndex.Set = c();
    DowName.Set = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

    for (i in 3:23)
    {
        for (j in c('firsthalf', 'secondhalf'))
        {
            TimeIndex.Set = c(TimeIndex.Set, sprintf('%d:%s', i, j));
        }
    }

    Connection2SQL = Database.Open.SqlServer();

    for (i in TimeIndex.Set)
    {
        for (j in DowName.Set)
        {
            print(sprintf('TimeIndex = %s; DOW = %s;', i, j));
            PairsTraveltime = Get.ALLPairTraveltime(Connection2SQL, destColIndex = 45, destRowIndex = 47,
            timeIndex = i, dowName = j, numSampleThreshold = 5);
        }
    }

    close(Connection2SQL);
}


#SqliteName = 'Result/ttr_to_(col_45_row_47).sqlite';
#Connection = dbConnect(RSQLite::SQLite(), dbname = SqliteName, loadable.extensions = TRUE);
#spatialitestatus = dbGetQuery(Connection, "SELECT load_extension('D:/Software/spatialite/libspatialite-1.dll')");

#dbDisconnect(Connection);