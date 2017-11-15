library(lubridate)
library(RODBC);
library(RSQLite);


source('0_DatabaseOperation.R');
source('0_Utility.R')



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


# UnitTest ----
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