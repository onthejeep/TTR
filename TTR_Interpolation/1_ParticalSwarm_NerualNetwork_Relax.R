library(ggplot2)
library(lubridate)
library(rgdal)

source('0_Utility.R');
source('1_Scaler.R');
source('1_ActivationFunction.R');

setwd('D:/MySVN/UA-Research/Dr Xia/R Code/TTR_Interpolation/TTR_Interpolation')

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

    InitialSolution = list();
    for (i in 1:(length(nnStructure) - 1))
    {
        NumRow = nnStructure[i];
        NumCol = nnStructure[i + 1];

        Combo = list();
        Combo$Weight = matrix(runif(NumRow * NumCol, min = -6, max = 6), nrow = NumRow, ncol = NumCol);
        Combo$Connectivity = matrix(1, nrow = NumRow, ncol = NumCol);
        InitialSolution[[i]] = Combo;
    }
    return(InitialSolution);
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
    Error = sqrt(sum((prediction - output) ^ 2) / nrow(prediction));

    # 0.5 * (DesiredData ¨C CalculatedData)^2
    # Error = 0.5 * sum((prediction - output) ^ 2);
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

PSO.NewGeneration.Parallel = function(originalGen, velocity, inertiaWeight, inertiaConnectivity,
                                      globalBestSolution, globalBestFitness, input, output)
{
    NumSolution = length(originalGen);

    SortedFitness = PSO.EvaluateGeneration(originalGen, input, output);
    SortedOriginalGen = PSO.SortGeneration(originalGen, SortedFitness$ix);

    CurrentBestSolution = PSO.BestSolution(SortedOriginalGen);
    CurrentBestFitness = PSO.BestFitness(SortedFitness$x);

    # Parallel computing - parameter setup
    ParallelComputingSplit = PSO.Isolation.ParameterSetup(originalGen, velocity, inertiaWeight, inertiaConnectivity,
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

PSO.Isolation.ParameterSetup = function(originalGen, velocity, inertiaWeight, inertiaConnectivity,
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
        Split$InertiaConnectivity = inertiaConnectivity;
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
    #paras$InertiaConnectivity
    #paras$CurrentBestSolution
    #paras$GlobalBestSolution

    #Output
    Velocity.Solution = list();
    UpdatedSolution = list();

    C1 = 1.49445;
    C2 = 1.49445;

    for (j in 1:length(paras$Origin.Solution))
    {
        NumRow = nrow(paras$Origin.Solution[[j]]$Weight);
        NumCol = ncol(paras$Origin.Solution[[j]]$Weight);

        VelCombo = list();
        VelCombo$Weight = paras$InertiaWeight * paras$Origin.Velocity4Solution[[j]]$Weight +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$CurrentBestSolution[[j]]$Weight - paras$Origin.Solution[[j]]$Weight) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$GlobalBestSolution[[j]]$Weight - paras$Origin.Solution[[j]]$Weight);

        VelCombo$Weight = apply(VelCombo$Weight, c(1, 2), FUN = Threshold.Velocity, 4);

        VelCombo$Connectivity = paras$Origin.Velocity4Solution[[j]]$Connectivity +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$CurrentBestSolution[[j]]$Connectivity - paras$Origin.Solution[[j]]$Connectivity) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (paras$GlobalBestSolution[[j]]$Connectivity - paras$Origin.Solution[[j]]$Connectivity);

        VelCombo$Connectivity = apply(VelCombo$Connectivity, c(1, 2), FUN = Threshold.Velocity, 6);

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

PSO.NewGeneration = function(originalGen, velocity, inertiaWeight, globalBestSolution, globalBestFitness, input, output)
{
    NumSolution = length(originalGen);

    SortedFitness = PSO.EvaluateGeneration(originalGen, input, output);
    SortedOriginalGen = PSO.SortGeneration(originalGen, SortedFitness$ix);

    CurrentBestSolution = PSO.BestSolution(SortedOriginalGen);
    CurrentBestFitness = PSO.BestFitness(SortedFitness$x);

    C1 = 2;
    C2 = 2;

    Velocity.Generation = list();
    for (i in 1:NumSolution)
    {
        SingleSolution = originalGen[[i]];
        Velocity.Solution = list();
        for (j in 1: length(SingleSolution))
        {
            NumRow = nrow(SingleSolution[[j]]$Weight);
            NumCol = ncol(SingleSolution[[j]]$Weight);

            Combo = list();

            Combo$Weight = inertiaWeight * velocity[[i]][[j]]$Weight +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (CurrentBestSolution[[j]]$Weight - SingleSolution[[j]]$Weight) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (globalBestSolution[[j]]$Weight - SingleSolution[[j]]$Weight);

            Combo$Connectivity = inertiaWeight * velocity[[i]][[j]]$Connectivity +
                    C1 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (CurrentBestSolution[[j]]$Connectivity - SingleSolution[[j]]$Connectivity) +
                    C2 * matrix(data = runif(n = NumRow * NumCol), nrow = NumRow, ncol = NumCol) * (globalBestSolution[[j]]$Connectivity - SingleSolution[[j]]$Connectivity);

            Combo$Connectivity = apply(Combo$Connectivity, c(1, 2), FUN = Activation.Binary, 0);

            Velocity.Solution[[j]] = Combo;

            originalGen[[i]][[j]]$Weight = originalGen[[i]][[j]]$Weight + Velocity.Solution[[j]]$Weight;
            originalGen[[i]][[j]]$Connectivity = originalGen[[i]][[j]]$Connectivity + Velocity.Solution[[j]]$Connectivity;
        }

        Velocity.Generation[[i]] = Velocity.Solution;
    }

    Result = list();
    if (CurrentBestFitness < globalBestFitness)
    {
        Result$GlobalBestSolution = CurrentBestSolution;
        Result$GlobalBestFitness = CurrentBestFitness;
        Result$OriginalGen = originalGen;
        Result$Velocity = Velocity.Generation;
    }
    else
    {
        Result$GlobalBestSolution = globalBestSolution;
        Result$GlobalBestFitness = globalBestFitness;
        Result$OriginalGen = originalGen;
        Result$Velocity = Velocity.Generation;
    }
    return(Result);
}


PSO.Execute = function(input, output)
{
    NNStructure = c(ncol(input), 16, 16, 8, ncol(output));

    TrackBestFitness = c();
    InitialGeneration = PSO.InitializeGeneration(numSolution = 90, nnStructure = NNStructure);
    SortedFitness = PSO.EvaluateGeneration(InitialGeneration, input, output);
    SortedGeneration = PSO.SortGeneration(InitialGeneration, SortedFitness$ix);

    GlobalBestSolution = PSO.BestSolution(SortedGeneration);
    GlobalBestFitness = PSO.BestFitness(SortedFitness$x);
    TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);

    NumIteration = 2000;
    Velocity = PSO.InitializeVelocity(InitialGeneration);
    InertiaWeight = seq(from = 0.5, to = 0.1, length = NumIteration);
    InertiaConnectivity = seq(from = 0.01, to = 0.01, length = NumIteration);

    for (i in 1:NumIteration)
    {
        #Result = PSO.NewGeneration(InitialGeneration, Velocity, InertiaWeight[i],
                #GlobalBestSolution, GlobalBestFitness, input, output);

        Result = PSO.NewGeneration.Parallel(InitialGeneration, Velocity, InertiaWeight[i], InertiaConnectivity[i],
            GlobalBestSolution, GlobalBestFitness, input, output);

        GlobalBestSolution = Result$GlobalBestSolution;
        GlobalBestFitness = Result$GlobalBestFitness;
        InitialGeneration = Result$OriginalGen;
        Velocity = Result$Velocity;

        TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);

        #print(paste(c('global best solution = ', GlobalBestSolution,
        #'global best fitness = ', GlobalBestFitness), collapse = ' '));
    }

    TrackBestFitness = as.data.frame(cbind(1:length(TrackBestFitness), TrackBestFitness));
    colnames(TrackBestFitness) = c('Iteration', 'Fitness');

    Result = list();
    Result$TrackBestFitness = TrackBestFitness;
    Result$BestSolution = GlobalBestSolution;
    return(Result);
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
    #Output = matrix(data = Value.Transform, nrow = length(Value.Transform));
    Output = cbind(Avg.Transform);
    #, Median.Transform

    Result = PSO.Execute(Input, Output);
    P1 = ggplot() +
        geom_line(data = Result$TrackBestFitness, aes(x = Iteration, y = Fitness), col = 'darkblue', size = 2) +
        ggtitle('Best fitness');
    print(P1);

    Prediction = PSO.EvaluateSolution(Input, Result$BestSolution);
    Avg.Inverse = MinMaxScaler.Inverse(Prediction[, 1], Avg.Scaler);
    #Median.Inverse = MinMaxScaler.Inverse(Prediction[, 2], Median.Scaler);

    Avg.Difference = Avg.Inverse - Sub.Grid[, 'ti_18_avg'];
    #Median.Difference = Median.Inverse - Sub.Grid[, 'ti_18_p90'];
    Comparison = cbind(Sub.Grid[, SelectedColumn], Avg.Difference);
    #, Median.Difference
    print(Comparison);

    print(sprintf('BestFitness = %0.4f; sum(abs(Avg.Difference)) = %0.4f',
        tail(Result$TrackBestFitness, n = 1)[2], sum(abs(Avg.Difference))));
    print(Result$BestSolution);
}

GlobalFunctions = ls(.GlobalEnv);
clusterExport(GlobalLocalCluster, list(GlobalFunctions, 'Threshold.Velocity', 'PSO.UpdateBinary',
    'Activation.Logistic'));

#set.seed(49);
#Input = matrix(sample.int(5, size = 12, replace = T), nrow = 4, ncol = 3);
#A = matrix(sample.int(20, size = 12, replace = T), nrow = 3, ncol = 4);
#Connectivity = matrix(sample(c(0, 1), size = 12, replace = T), nrow = 3);

#Input %*% (A * Connectivity

# save(file = 'Sub.Grid.rdata', Sub.Grid)