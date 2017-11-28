library(lubridate)
library(RODBC);
library(RSQLite);

source('0_Utility.R')

Alter.SqliteTable.AddColumn = function()
{
    GridDatabase = 'Result/ttr_to_(col_45_row_47).sqlite';
    Connection = Database.Open.Sqlite(GridDatabase);
    dbBegin(Connection);

    for (ti in 1:48)
    {
        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_tuesday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_wednesday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_thursday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_friday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_saturday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_sunday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);
    }

    dbCommit(Connection);
    dbDisconnect(Connection);
}

# Database Operation ----
Alter.SqliteTable = function()
{
    GridDatabase = 'Result/ttr_to_(col_45_row_47).sqlite';
    Connection = Database.Open.Sqlite(GridDatabase);

    # dbSendQuery(Connection, "select load_extension('Config/libspatialite-1.dll')");

    dbBegin(Connection);
    for (ti in 1:48)
    {
        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_sz int', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_avg float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_median float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_std float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_cov float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_p90 float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_p95 float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_bt float', ti);
        dbSendQuery(Connection, AlterStatement);

        AlterStatement = sprintf('alter table grid_kunshan_monday add column ti_%d_pt float', ti);
        dbSendQuery(Connection, AlterStatement);

    }
    dbCommit(Connection);

    DOW = c('tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday');
    for (dow in DOW)
    {
        CopyStatement = sprintf('CREATE TABLE grid_kunshan_%s AS SELECT * FROM grid_kunshan_monday', dow);
        dbSendQuery(Connection, CopyStatement);
    }
    dbCommit(Connection);

    #for (i in 1:length(DOW))
    #{
        #InsertStatement = sprintf('insert into geometry_columns (f_table_name, f_geometry_column,
        #type, coord_dimension, srid, spatial_index_enabled) values 
        #(\'grid_kunshan_%s\', \'Geometry\', \'POLYGON\', 2, 4326, 0)', 
        #DOW[i]);
        #print(InsertStatement);
        #dbSendQuery(Connection, InsertStatement);
    #}
    #dbCommit(Connection);
    dbDisconnect(Connection);
}

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