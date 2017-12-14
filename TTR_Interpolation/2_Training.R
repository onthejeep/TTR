

#source('0_ParallelComputingSetup.R');




PSO.NeuralNetwork.ST.Intepolation = function()
{
    StartTime = now();
    print(StartTime);

    # setup data
    load('TrainTestData/Train.rdata'); # Train
    SelectedColumn = c('avg', 'std', 'cov', 'p90', 'p95', 'bt', 'pt');

    # input normalization
    TI.Scaler = MinMaxScaler(Train[, 'TimeIndex']);
    TI.Transform = MinMaxScaler.Transform(Train[, 'TimeIndex'], TI.Scaler);

    Dis.Scaler = MinMaxScaler(Train[, 'Distance']);
    Dis.Transform = MinMaxScaler.Transform(Train[, 'Distance'], Dis.Scaler);

    Col.Scaler = MinMaxScaler(Train[, 'Col']);
    Col.Transform = MinMaxScaler.Transform(Train[, 'Col'], Col.Scaler);

    Row.Scaler = MinMaxScaler(Train[, 'Row']);
    Row.Transform = MinMaxScaler.Transform(Train[, 'Row'], Row.Scaler);

    # output normalization
    Avg.Scaler = MinMaxScaler(Train[, 'avg']);
    Avg.Transform = MinMaxScaler.Transform(Train[, 'avg'], Avg.Scaler);

    Std.Scaler = MinMaxScaler(Train[, 'std']);
    Std.Transform = MinMaxScaler.Transform(Train[, 'std'], Std.Scaler);

    Cov.Scaler = MinMaxScaler(Train[, 'cov']);
    Cov.Transform = MinMaxScaler.Transform(Train[, 'cov'], Cov.Scaler);

    P90.Scaler = MinMaxScaler(Train[, 'p90']);
    P90.Transform = MinMaxScaler.Transform(Train[, 'p90'], P90.Scaler);

    P95.Scaler = MinMaxScaler(Train[, 'p95']);
    P95.Transform = MinMaxScaler.Transform(Train[, 'p95'], P95.Scaler);

    Bt.Scaler = MinMaxScaler(Train[, 'bt']);
    Bt.Transform = MinMaxScaler.Transform(Train[, 'bt'], Bt.Scaler);

    Pt.Scaler = MinMaxScaler(Train[, 'pt']);
    Pt.Transform = MinMaxScaler.Transform(Train[, 'pt'], Pt.Scaler);

    Input = cbind(TI.Transform, Col.Transform, Row.Transform, Dis.Transform);
    Output = cbind(Avg.Transform);
    # , Std.Transform, Cov.Transform,  P90.Transform, P95.Transform, Bt.Transform, Pt.Transform

    # train neural network
    Boot.TrainedNeuralNetwork = Train.Bagging.NerualNetwork(Input, Output);

    # predict values
    Prediction = Bagging.NerualNetwork.Prediction(Input, Boot.TrainedNeuralNetwork);

    save(file = 'TrainTestData/Prediction.rdata', Prediction);

    EndTime = now();
    print(sprintf('running time = %0.2f minutes', difftime(EndTime, StartTime, units = 'mins')));
}