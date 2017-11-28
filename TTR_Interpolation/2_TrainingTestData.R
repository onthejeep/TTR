library(lubridate)
library(RSQLite);

source('0_Utility.R');
source('0_DatabaseOperation.R');



TrainTestData = function()
{
    TimeIndex = TrainTestData.TimeIndex();

    TrainTest = c();
    for (ti in TimeIndex)
    {
        Temp = Data.TimeIndex(ti);
        TrainTest = rbind(TrainTest, Temp);
    }
    save(file = 'TrainTestData/TrainTest.rdata', TrainTest);
    #return(TrainTest);
}

TrainData = function()
{
    TimeIndex = TrainData.TimeIndex();

    Train = c();
    for (ti in TimeIndex)
    {
        Temp = Data.TimeIndex(ti);
        Train = rbind(Train, Temp);
    }
    save(file = 'TrainTestData/Train.rdata', Train);
    #return(TrainTest);
}

TestData = function()
{
    TimeIndex = TestData.TimeIndex();

    Test = c();
    for (ti in TimeIndex)
    {
        Temp = Data.TimeIndex(ti);
        Test = rbind(Test, Temp);
    }
    save(file = 'TrainTestData/Test.rdata', Test);
    #return(TrainTest);
}


Data.TimeIndex = function(ti)
{
    GridDatabase = 'Result/ttr_to_(col_45_row_47).sqlite';
    Connection = Database.Open.Sqlite(GridDatabase);

    ColumnNames = ColumnName.TimeIndex(ti);
    SelectCommand = sprintf('select %s from grid_kunshan_tuesday where ti_%d_sz > 5', ColumnNames, ti);
    print(SelectCommand);
    Result = dbGetQuery(Connection, SelectCommand);

    dbDisconnect(Connection);

    TiColRowDistance = c();
    for (i in 1:nrow(Result))
    {
        Temp = Find.Row.Col(Result[i, 'id']);
        Distance = abs(Temp[1] - 45) + abs(Temp[2] - 47);
        Temp = c(ti, Temp, Distance);
        TiColRowDistance = rbind(TiColRowDistance, Temp);
    }

    colnames(TiColRowDistance) = c('TimeIndex', 'Col', 'Row', 'Distance');

    Result = cbind(TiColRowDistance, Result);
    rownames(Result) = c();
    save(file = sprintf('TrainTestData/Ti_%02d.rdata', ti), Result);
    return(Result);
}

ColumnName.TimeIndex = function(ti)
{
    ColumnName = sprintf('id, ti_%d_sz as sz, ti_%d_avg as avg, ti_%d_std as std, ti_%d_cov as cov, 
    ti_%d_p90 as p90, ti_%d_p95 as p95, ti_%d_bt as bt, ti_%d_pt as pt', ti, ti, ti, ti, ti, ti, ti, ti);
    return(ColumnName);
}

TrainData.TimeIndex = function()
{
    return(c(29, 31, 33, 35, 36, 37, 39, 40));
}

TestData.TimeIndex = function()
{
    return(c(30, 32, 34, 38));
}

TrainTestData.TimeIndex = function()
{
    TimeIndex = c();
    StartHour = 14;
    EndHour = 19;

    for (i in StartHour:EndHour)
    {
        for (j in c('firsthalf', 'secondhalf'))
        {
            Temp = sprintf('%d:%s', i, j);
            TimeIndex = c(TimeIndex, Find.TimeIndex(Temp));
        }
    }

    return(TimeIndex);
}
