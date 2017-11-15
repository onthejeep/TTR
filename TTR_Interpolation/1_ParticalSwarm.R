library(ggplot2)
library(lubridate)


PSO.InitializeVelocity = function(generation)
{
    Velocity = matrix(0, nrow = nrow(generation), ncol = ncol(generation));
    return(Velocity);
}
PSO.InitializeGeneration = function(numSolution)
{
    X = runif(n = numSolution, min = -5, max = 5);
    Y = runif(n = numSolution, min = -5, max = 5);
    Z = runif(n = numSolution, min = -5, max = 5);

    InitialGerantion = data.frame(X, Y, Z);
    return(InitialGerantion);
}

PSO.EvaluateSolution = function(singleSolution)
{
    Result = singleSolution$X ^ 2 + singleSolution$Y ^ 2 + singleSolution$Z ^ 2;
    return(Result);
}

PSO.EvaluateGeneration = function(generation)
{
    Fitness = c();

    for (i in 1: nrow(generation))
    {
        Fitness[i] = PSO.EvaluateSolution(generation[i,]);
    }

    SortedFitness = sort(Fitness, index.return = T);
    return(SortedFitness);
}

PSO.SortGeneration = function(originalGen, sortIndex)
{
    return(originalGen[sortIndex, ]);
}
PSO.BestSolution = function(sortedGen)
{
    return(sortedGen[1, ]);
}
PSO.BestFitness = function(sortedFitness)
{
    return(sortedFitness[1]);
}

PSO.NewGeneration = function(originalGen, velocity, inertiaWeight, globalBestSolution, globalBestFitness)
{
    NumSolution = nrow(originalGen);
    NumVariable = ncol(originalGen);

    SortedFitness = PSO.EvaluateGeneration(originalGen);
    SortedOriginalGen = PSO.SortGeneration(originalGen, SortedFitness$ix);

    CurrentBestSolution = PSO.BestSolution(SortedOriginalGen);
    CurrentBestFitness = PSO.BestFitness(SortedFitness$x);

    C1 = 2;
    C2 = 2;

    Velocity = c();
    for (i in 1: NumSolution)
    {
        Velocity.Solution = inertiaWeight * velocity[i,] +
                    C1 * runif(n = NumVariable) * (CurrentBestSolution - originalGen[i,]) +
                    C2 * runif(n = NumVariable) * (globalBestSolution - originalGen[i,]);

        originalGen[i,] = originalGen[i,] + Velocity.Solution;
        Velocity = rbind(Velocity, Velocity.Solution);
    }

    Result = list();
    if (CurrentBestFitness < globalBestFitness)
    {
        Result$GlobalBestSolution = CurrentBestSolution;
        Result$GlobalBestFitness = CurrentBestFitness;
        Result$OriginalGen = originalGen;
        Result$Velocity = Velocity;
    }
    else
    {
        Result$GlobalBestSolution = globalBestSolution;
        Result$GlobalBestFitness = globalBestFitness;
        Result$OriginalGen = originalGen;
        Result$Velocity = Velocity;
    }
    return(Result);
}

PSO.UnitTest = function()
{
    TrackBestFitness = c();

    InitialGeneration = PSO.InitializeGeneration(numSolution = 100);
    SortedFitness = PSO.EvaluateGeneration(InitialGeneration);
    SortedGeneration = PSO.SortGeneration(InitialGeneration, SortedFitness$ix);

    GlobalBestSolution = PSO.BestSolution(SortedGeneration);
    GlobalBestFitness = PSO.BestFitness(SortedFitness$x);
    TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);

    NumIteration = 100;
    Velocity = PSO.InitializeVelocity(InitialGeneration);
    InertiaWeight = seq(from = 0.2, to = 0.01, length = NumIteration);

    for (i in 1: NumIteration)
    {
        Result = PSO.NewGeneration(InitialGeneration, Velocity, InertiaWeight[i],
                    GlobalBestSolution, GlobalBestFitness);

        GlobalBestSolution = Result$GlobalBestSolution;
        GlobalBestFitness = Result$GlobalBestFitness;
        InitialGeneration = Result$OriginalGen;
        Velocity = Result$Velocity;

        TrackBestFitness = c(TrackBestFitness, GlobalBestFitness);

        print(paste(c('global best solution = ', GlobalBestSolution, 
            'global best fitness = ', GlobalBestFitness), collapse = ' '));
    }

    TrackBestFitness = as.data.frame(cbind(1:length(TrackBestFitness), TrackBestFitness));
    colnames(TrackBestFitness) = c('order', 'value');

    ggplot() +
        geom_line(data = TrackBestFitness, aes(x = order, y = value), col = 'darkblue', size = 2) +
        ggtitle('Best fitness');

    print(now());
}