

Parse.Structure = function(structure = '32,32,16,8')
{
    SplitStr = strsplit(structure, split = ',', fixed = TRUE);
    Structure = as.numeric(unlist(SplitStr));
    return(Structure);
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
           'firsthalf' = { NumMinute = 1 },
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

Find.Row.Col = function(fid)
{
    Col = ceiling(fid / 100);
    Row = (fid - 100 * Col) %% 100 + 1;

    return(c(Col, Row));
}

# ---- Unit Test ----
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