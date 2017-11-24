
source('0_Utility.R');
source('1_Scaler.R');
source('1_ActivationFunction.R');

PSO.InitializeVelocity = function(generation)
{
    Velocity.Generation = list();
    SingleSolutionStructure = generation[[1]];
    for (i in 1: length(generation))
    {
        Velocity.Solution = list();
        for (j in 1:length(SingleSolutionStructure))
        {
            NumRow = nrow(SingleSolutionStructure[[j]]$Weight);
            NumCol = ncol(SingleSolutionStructure[[j]]$Weight);

            Combo = list();
            Combo$Weight = matrix(0, nrow = NumRow, ncol = NumCol);
            Combo$Connectivity = matrix(0, nrow = NumRow, ncol = NumCol);

            Velocity.Solution[[j]] = Combo;
        }
        Velocity.Generation[[i]] = Velocity.Solution;
    }

    return(Velocity.Generation);
}
PSO.InitializeGeneration = function(numSolution = 100, nnStructure)
{
    InitialGeneration = list();
    for (i in 1: numSolution)
    {
        InitialGeneration[[i]] = PSO.InitializeSolution(nnStructure);
    }
    return(InitialGeneration);
}
PSO.InitializeSolution = function(nnStructure)
{
    # nnStructure is a vector
    # example: c(3, 7, 2) = (numInput = 3, hiddenNeuron = 7, numOutput = 2)
    # example: c(3, 7, 4, 2) = (numInput = 3, hidden1 = 7, hidden2 = 4, numOutput = 2)

    WeightMin = as.numeric(Config$model$neuralnetwork$solution$initialweight$min$text);
    WeightMax = as.numeric(Config$model$neuralnetwork$solution$initialweight$max$text);

    InitialSolution = list();
    for (i in 1:(length(nnStructure) - 1))
    {
        NumRow = nnStructure[i];
        NumCol = nnStructure[i + 1];

        Combo = list();
        Combo$Weight = matrix(runif(NumRow * NumCol, min = WeightMin, max = WeightMax), nrow = NumRow, ncol = NumCol);
        Combo$Connectivity = matrix(1, nrow = NumRow, ncol = NumCol);
        InitialSolution[[i]] = Combo;
    }
    return(InitialSolution);
}

PSO.PickUpBestInitialGeneration = function(input, output, numSolution, nnStructure,
                                           errorThreshold = 0.14, maxTry = 100)
{
    # Parallel computing - parameter setup
    ParallelComputingSplit = PSO.InitialGeneration.ParameterSetup(input, output, nnStructure, maxTry);
    # Parallel computing - calcualtion
    SplitResult = parLapply(GlobalLocalCluster, X = ParallelComputingSplit, fun = PSO.InitialGeneration.ParallelComputing);

    Fitness = unlist(SplitResult);
    BestFitness = min(Fitness);
    BestFitness.Index = which.min(Fitness);

    if (BestFitness <= errorThreshold)
    {
        print(sprintf('The error threshold is reached at (%0.4f)', BestFitness));
    }
    else
    {
        print(sprintf('The lowest error is (%0.4f)', BestFitness));
    }

    return(ParallelComputingSplit[[BestFitness.Index]]$Generation);
    
}

PSO.InitialGeneration.ParameterSetup = function(input, output, nnStructure, maxTry = 100)
{
    ParallelComputingSplit = list();

    NumSolution = as.numeric(Config$model$neuralnetwork$numsolution$text);

    for (i in 1:maxTry)
    {
        Split = list();
        Split$Index = i;
        Split$Input = input;
        Split$Output = output;
        Split$Generation = PSO.InitializeGeneration(numSolution = NumSolution, nnStructure);

        ParallelComputingSplit[[i]] = Split;
    }

    return(ParallelComputingSplit);
}

PSO.InitialGeneration.ParallelComputing = function(paras)
{
    SortedFitness = PSO.EvaluateGeneration(paras$Generation, paras$Input, paras$Output);
    BestFitness = SortedFitness$x[1];

    return(BestFitness);
}

PSO.EvaluateSolution = function(input, singleSolution)
{
    # input is a vector
    # example: input = matrix(data = 1:3, nrow = 1, ncol = 3);

    Result = input;
    NumHiddenLayer = length(singleSolution) - 1;

    for (i in 1:NumHiddenLayer)
    {
        Result = Result %*% (singleSolution[[i]]$Weight * singleSolution[[i]]$Connectivity);
        Result = apply(Result, c(1, 2), FUN = Activation.Logistic);
    }
    
    Result = Result %*% (singleSolution[[NumHiddenLayer + 1]]$Weight * singleSolution[[NumHiddenLayer + 1]]$Connectivity);
    Result = apply(Result, c(1, 2), FUN = Activation.Logistic);
    
    return(Result);
}

PSO.Prediction2Classification = function(prediction)
{
    Classification = apply(prediction, 1, which.max);
    Classification = matrix(data = Classification, nrow = length(Classification));
    return(Classification);
}

PSO.EvaluateError = function(prediction, output)
{
    # root-mean-square error
    Error = sqrt(sum((prediction - output) ^ 2) / (nrow(prediction) * ncol(prediction)));
    return(Error);
}

PSO.EvaluateGeneration = function(generation, input, output)
{
    Fitness = c();

    for (i in 1:length(generation))
    {
        Prediction = PSO.EvaluateSolution(input, generation[[i]]);
        Fitness[i] = PSO.EvaluateError(Prediction, output);
    }

    SortedFitness = sort(Fitness, index.return = T);
    return(SortedFitness);
}

PSO.SortGeneration = function(originalGen, sortIndex)
{
    return(originalGen[sortIndex]);
}
PSO.BestSolution = function(sortedGen)
{
    return(sortedGen[[1]]);
}
PSO.BestFitness = function(sortedFitness)
{
    return(sortedFitness[1]);
}

PSO.NewGeneration.Parallel = function(originalGen, velocity, inertiaWeight,
                                      globalBestSolution, globalBestFitness, input, output)
{
    NumSolution = length(originalGen);

    SortedFitness = PSO.EvaluateGeneration(originalGen, input, output);
    SortedOriginalGen = PSO.SortGeneration(originalGen, SortedFitness$ix);

    CurrentBestSolution = PSO.BestSolution(SortedOriginalGen);
    CurrentBestFitness = PSO.BestFitness(SortedFitness$x);

    # Parallel computing - parameter setup
    ParallelComputingSplit = PSO.Isolation.ParameterSetup(originalGen, velocity, inertiaWeight,
        CurrentBestSolution, globalBestSolution);
    # Parallel computing - calcualtion
    SplitResult = parLapply(GlobalLocalCluster, X = ParallelComputingSplit, fun = PSO.Isolation.ParallelComputing);

    Velocity.Generation = list();
    UpdatedGeneration = list();
    for (i in 1:NumSolution)
    {
        Velocity.Generation[[i]] = SplitResult[[i]]$Velocity.Solution;
        UpdatedGeneration[[i]] = SplitResult[[i]]$UpdatedSolution;
    }

    Result = list();
    if (CurrentBestFitness < globalBestFitness)
    {
        Result$GlobalBestSolution = CurrentBestSolution;
        Result$GlobalBestFitness = CurrentBestFitness;
        Result$OriginalGen = UpdatedGeneration;
        Result$Velocity = Velocity.Generation;
    }
    else
    {
        Result$GlobalBestSolution = globalBestSolution;
        Result$GlobalBestFitness = globalBestFitness;
        Result$OriginalGen = UpdatedGeneration;
        Result$Velocity = Velocity.Generation;
    }
    return(Result);
}

PSO.Isolation.ParameterSetup = function(originalGen, velocity, inertiaWeight,
                                        currentBestSolution, globalBestSolution)
{
    ParallelComputingSplit = list();
    NumSolution = length(originalGen);

    for (i in 1:NumSolution)
    {
        Split = list();
        Split$Index = i;
        Split$Origin.Solution = originalGen[[i]];
        Split$Origin.Velocity4Solution = velocity[[i]];
        Split$InertiaWeight = inertiaWeight;
        Split$CurrentBestSolution = currentBestSolution;
        Split$GlobalBestSolution = globalBestSolution;

        ParallelComputingSplit[[i]] = Split;
    }

    return(ParallelComputingSplit);
}

PSO.Isolation.ParallelComputing = function(paras)
{
    #Input
    #paras$Origin.Solution
    #paras$Origin.Velocity4Solution
    #paras$InertiaWeight
    #paras$CurrentBestSolution
    #paras$GlobalBestSolution

    #Output
    Velocity.Solution = list();
    UpdatedSolution = list();

    C1 = as.numeric(Config$model$pso$C1$text);
    C2 = as.numeric(Config$model$pso$C2$text);

    for (j in 1:length(paras$Origin.Solution))
    {
        NumRow = nrow(paras$Origin.Solution[[j]]$Weight);
        NumCol = ncol(paras$Origin.Solution[[j]]$Weight);

        VelCombo = list();
        VelCombo$Weight = paras$InertiaWeight * paras$Origin.Velocity4Solution[[j]]$Weight +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$CurrentBestSolution[[j]]$Weight - paras$Origin.Solution[[j]]$Weight) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$GlobalBestSolution[[j]]$Weight - paras$Origin.Solution[[j]]$Weight);

        
        VelCombo$Weight = apply(VelCombo$Weight, c(1, 2), FUN = Threshold.Velocity, as.numeric(Config$model$pso$velocitythreshold$text));

        VelCombo$Connectivity = paras$Origin.Velocity4Solution[[j]]$Connectivity +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$CurrentBestSolution[[j]]$Connectivity - paras$Origin.Solution[[j]]$Connectivity) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$GlobalBestSolution[[j]]$Connectivity - paras$Origin.Solution[[j]]$Connectivity);

        VelCombo$Connectivity = apply(VelCombo$Connectivity, c(1, 2), FUN = Threshold.Velocity, as.numeric(Config$model$dpso$velocitythreshold$text));

        Velocity.Solution[[j]] = VelCombo;

        SolCombo = list();
        SolCombo$Weight = paras$Origin.Solution[[j]]$Weight + VelCombo$Weight;
        SolCombo$Connectivity = apply(VelCombo$Connectivity, c(1, 2), FUN = PSO.UpdateBinary);
        
        UpdatedSolution[[j]] = SolCombo;
    }

    Result = list();
    Result$Velocity.Solution = Velocity.Solution;
    Result$UpdatedSolution = UpdatedSolution;
    return(Result);
}

PSO.UpdateBinary = function(x)
{
    Transform = Activation.Logistic(x);
    ifelse(runif(n = 1) < Transform, return(1), return(0));
}


PSO.Execute = function(input, output)
{
    TrackBestFitness = c();

    NNStructure = Parse.Structure(Config$model$neuralnetwork$structure$text);
    NNStructure = c(ncol(input), NNStructure, ncol(output));

    NumSolution = as.numeric(Config$model$neuralnetwork$numsolution$text);
    ErrorThreshold = as.numeric(Config$model$neuralnetwork$initialgeneration$initialerror$text);
    MaxTry = as.numeric(Config$model$neuralnetwork$initialgeneration$maxtry$text);
    InitialGeneration = PSO.PickUpBestInitialGeneration(input, output,
                                                        numSolution = NumSolution,
                                                        nnStructure = NNStructure,
                                                        errorThreshold = ErrorThreshold,
                                                        maxTry = MaxTry);

    SortedFitness = PSO.EvaluateGeneration(InitialGeneration, input, output);
    SortedGeneration = PSO.SortGeneration(InitialGeneration, SortedFitness$ix);

    GlobalBestSolution = PSO.BestSolution(SortedGeneration);
    GlobalBestFitness = PSO.BestFitness(SortedFitness$x);
    TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);

    NumIteration = as.numeric(Config$model$pso$iteration$text);
    Velocity = PSO.InitializeVelocity(InitialGeneration);
    InertiaWeight = seq(from = as.numeric(Config$model$pso$inertiaweight$from$text),
                        to = as.numeric(Config$model$pso$inertiaweight$to$text), length = NumIteration);

    for (i in 1:NumIteration)
    {
        Result = PSO.NewGeneration.Parallel(InitialGeneration, Velocity, InertiaWeight[i],
            GlobalBestSolution, GlobalBestFitness, input, output);

        GlobalBestSolution = Result$GlobalBestSolution;
        GlobalBestFitness = Result$GlobalBestFitness;
        InitialGeneration = Result$OriginalGen;
        Velocity = Result$Velocity;

        TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);
    }

    TrackBestFitness = as.data.frame(cbind(1:length(TrackBestFitness), TrackBestFitness));
    colnames(TrackBestFitness) = c('Iteration', 'TrainingError');

    Result = list();
    Result$TrackBestFitness = TrackBestFitness;
    Result$BestSolution = GlobalBestSolution;
    return(Result);
}

Bagging.NerualNetwork = function(input, output)
{
    NumBoot = as.numeric(Config$model$bagging$numBootstrap$text);
    Bagging.TrainedNN = list();

    for (i in 1: NumBoot)
    {
        Index = sample(1:nrow(input), size = nrow(input), replace = T);
        BootInput = input[Index,];
        BootOutput = as.matrix(output[Index,], nrow = nrow(output));

        Bagging.TrainedNN[[i]] = PSO.Execute(BootInput, BootOutput);
    }

    save(file = 'Result/Bagging.TrainedNN.rdata', Bagging.TrainedNN);
    return(Bagging.TrainedNN);
}

Bagging.NerualNetwork.Prediction = function(input, bagging.TrainedNN)
{
    BootPrediction = PSO.EvaluateSolution(input, bagging.TrainedNN[[1]]$BestSolution);

    for (i in 2:length(bagging.TrainedNN))
    {
        Prediction = PSO.EvaluateSolution(input, bagging.TrainedNN[[i]]$BestSolution);
        BootPrediction = BootPrediction + Prediction;
    }
    return(BootPrediction / length(bagging.TrainedNN));
}

PSO.UnitTest1 = function()
{
    # Input
    Input = matrix(data = c(0.1, 0.1,
    0.2, 0.2,
    0.1, 0.3,
    0.3, 0.1,
    0.7, 0.1,
    0.8, 0.3,
    0.9, 0.2,
    0.6, 0.4,
    0.7, 0.7,
    0.8, 0.9,
    0.6, 0.8,
    0.1, 0.9,
    0.2, 0.7,
    0.3, 0.8), nrow = 14, ncol = 2, byrow = T);
    
    # Output
    Output = matrix(data = c(1, 0, 0, 0,
                    1, 0, 0, 0,
                    1, 0, 0, 0,
                    1, 0, 0, 0,
                    0, 1, 0, 0,
                    0, 1, 0, 0,
                    0, 1, 0, 0,
                    0, 1, 0, 0,
                    0, 0, 1, 0,
                    0, 0, 1, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1,
                    0, 0, 0, 1,
    0, 0, 0, 1), nrow = 14, ncol = 4, byrow = T);

    Result = PSO.Execute(Input, Output);

    P1 = ggplot() +
        geom_line(data = Result$TrackBestFitness, aes(x = Iteration, y = Fitness), col = 'darkblue', size = 2) +
        ggtitle('Best fitness');
    print(P1);

    Prediction = PSO.EvaluateSolution(Input, Result$BestSolution);
    Classification = PSO.Prediction2Classification(Prediction);
    print(Classification);
}

PSO.UnitTest2 = function()
{
    # Input
    Input = matrix(data = c(0.2, 0.5,
    1, 0,
    2, 0,
    2, 2,
    1, 2,
    5, 6,
    6, 6,
    6, 7,
    8, 7,
    7, 7,
    7, 6,
    0, 15,
    1, 14,
    2, 13,
    3, 12,
    4, 16,
    5, 15,
    6, 14,
    6, 13,
    7, 15,
    8, 15), nrow = 21, ncol = 2, byrow = T);

    # Output
    Output = matrix(data = c(1, 0, 0,
    1, 0, 0, 
    1, 0, 0, 
    1, 0, 0,
    1, 0, 0, 
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
    0, 1, 0,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1,
    0, 0, 1), nrow = 21, ncol = 3, byrow = T);

    Result = PSO.Execute(Input, Output);

    P1 = ggplot() +
        geom_line(data = Result$TrackBestFitness, aes(x = Iteration, y = Fitness), col = 'darkblue', size = 2) +
        ggtitle('Best fitness');
    print(P1);

    Prediction = PSO.EvaluateSolution(Input, Result$BestSolution);
    Classification = PSO.Prediction2Classification(Prediction);
    print(Classification);
}


PSO.UnitTest3 = function()
{
    #TTR.Grid = readOGR(dsn = 'result/ttr_to_(col_45_row_47).sqlite', layer = 'grid_kunshan_monday');
    #SelectedColumn = c('ti_18_avg', 'ti_18_p90');
    #Sub.Grid = subset(TTR.Grid[, c('id', SelectedColumn)], TTR.Grid[, SelectedColumn]$ti_18_avg >= 0);
    #Sub.Grid = as.data.frame(Sub.Grid);

    #ColRow = c();
    #for (i in 1:nrow(Sub.Grid))
    #{
        #Temp = Find.Row.Col(Sub.Grid[i, 'id']);
        #ColRow = rbind(ColRow, Temp);
    #}
    #colnames(ColRow) = c('Col', 'Row');
    #Sub.Grid = cbind(Sub.Grid, ColRow);

    StartTime = now();

    # setup data
    load('Sub.Grid.rdata');
    SelectedColumn = c('ti_18_avg');

    Distance = abs(Sub.Grid[, 'Col'] - 45) + abs(Sub.Grid[, 'Row'] - 47);
    Dis.Scaler = MinMaxScaler(Distance, range = c(0, 1));
    Dis.Transform = MinMaxScaler.Transform(Distance, Dis.Scaler);

    Col.Scaler = MinMaxScaler(Sub.Grid[, 'Col'], range = c(0, 1));
    Col.Transform = MinMaxScaler.Transform(Sub.Grid[, 'Col'], Col.Scaler);
    Row.Scaler = MinMaxScaler(Sub.Grid[, 'Row'], range = c(0, 1));
    Row.Transform = MinMaxScaler.Transform(Sub.Grid[, 'Row'], Row.Scaler);
    Avg.Scaler = MinMaxScaler(Sub.Grid[, 'ti_18_avg'], range = c(0, 1));
    Avg.Transform = MinMaxScaler.Transform(Sub.Grid[, 'ti_18_avg'], Avg.Scaler);
    Median.Scaler = MinMaxScaler(Sub.Grid[, 'ti_18_p90'], range = c(0, 1));
    Median.Transform = MinMaxScaler.Transform(Sub.Grid[, 'ti_18_p90'], Median.Scaler);

    Input = cbind(Col.Transform, Row.Transform, Dis.Transform);
    Output = cbind(Avg.Transform);

    # train neural network
    TrainedNeuralNetwork = PSO.Execute(Input, Output);
    save(file = 'Result/TrainedNeuralNetwork.rdata', TrainedNeuralNetwork);

    # plot tracked fitness 
    P1 = ggplot() +
        geom_line(data = TrainedNeuralNetwork$TrackBestFitness, aes(x = Iteration, y = TrainingError), col = 'darkblue', size = 2);
    print(P1);

    # predict values
    Prediction = PSO.EvaluateSolution(Input, TrainedNeuralNetwork$BestSolution);

    # value transform
    Avg.Inverse = MinMaxScaler.Inverse(Prediction[, 1], Avg.Scaler);

    # comparison: groud truth vs. prediction
    Avg.Difference = Avg.Inverse - Sub.Grid[, 'ti_18_avg'];
    Comparison = cbind(Sub.Grid[, SelectedColumn], Avg.Difference);
    print(Comparison);

    print(sprintf('BestFitness = %0.4f; sum(abs(Avg.Difference)) = %0.4f',
        tail(TrainedNeuralNetwork$TrackBestFitness, n = 1)[2], sum(abs(Avg.Difference))));

    EndTime = now();
    print(sprintf('running time = %0.2f minutes', difftime(EndTime, StartTime, units = 'mins')));
}

PSO.UnitTest4 = function()
{
    StartTime = now();

    # setup data
    load('Sub.Grid.rdata');
    SelectedColumn = c('ti_18_avg');

    Distance = abs(Sub.Grid[, 'Col'] - 45) + abs(Sub.Grid[, 'Row'] - 47);
    Dis.Scaler = MinMaxScaler(Distance, range = c(0, 1));
    Dis.Transform = MinMaxScaler.Transform(Distance, Dis.Scaler);

    Col.Scaler = MinMaxScaler(Sub.Grid[, 'Col'], range = c(0, 1));
    Col.Transform = MinMaxScaler.Transform(Sub.Grid[, 'Col'], Col.Scaler);
    Row.Scaler = MinMaxScaler(Sub.Grid[, 'Row'], range = c(0, 1));
    Row.Transform = MinMaxScaler.Transform(Sub.Grid[, 'Row'], Row.Scaler);
    Avg.Scaler = MinMaxScaler(Sub.Grid[, 'ti_18_avg'], range = c(0, 1));
    Avg.Transform = MinMaxScaler.Transform(Sub.Grid[, 'ti_18_avg'], Avg.Scaler);
    Median.Scaler = MinMaxScaler(Sub.Grid[, 'ti_18_p90'], range = c(0, 1));
    Median.Transform = MinMaxScaler.Transform(Sub.Grid[, 'ti_18_p90'], Median.Scaler);

    Input = cbind(Col.Transform, Row.Transform, Dis.Transform);
    Output = cbind(Avg.Transform);

    # train neural network
    Boot.TrainedNeuralNetwork = Bagging.NerualNetwork(Input, Output);

    # predict values
    Prediction = Bagging.NerualNetwork.Prediction(Input, Boot.TrainedNeuralNetwork);

    # value transform
    Avg.Inverse = MinMaxScaler.Inverse(Prediction[, 1], Avg.Scaler);

    # comparison: groud truth vs. prediction
    Avg.Difference = Avg.Inverse - Sub.Grid[, 'ti_18_avg'];
    Comparison = cbind(Sub.Grid[, SelectedColumn], Avg.Difference);
    print(Comparison);

    EndTime = now();
    print(sprintf('running time = %0.2f minutes', difftime(EndTime, StartTime, units = 'mins')));
}

#
GlobalFunctions = ls(.GlobalEnv);
clusterExport(GlobalLocalCluster, list(GlobalFunctions, 'Threshold.Velocity', 'PSO.UpdateBinary',
    'Activation.Logistic', 'Config', 'PSO.EvaluateGeneration', 'PSO.EvaluateSolution', 'PSO.EvaluateError'));