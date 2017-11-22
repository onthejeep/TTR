

MinMaxScaler.Transform = function(x, scaler)
{
    Scaled = scaler$TransformRange * (x - scaler$Min) / scaler$Range + scaler$TransformMin;
}

MinMaxScaler = function(x, range = c(-1, 1))
{
    MinValue = min(x);
    MaxValue = max(x);

    Scaler = list();
    Scaler$Min = MinValue;
    Scaler$Max = MaxValue;
    Scaler$Range = MaxValue - MinValue;
    Scaler$TransformMin = range[1];
    Scaler$TransformMax = range[2];
    Scaler$TransformRange = range[2] - range[1];
    return(Scaler);
}

MinMaxScaler.Inverse = function(x, scaler)
{
    Scaled = (x - scaler$TransformMin) * scaler$Range / scaler$TransformRange + scaler$Min;
}

NormalScaler = function(x)
{
    Avg = mean(x);
    Std = sd(x);
    Scaler = list();
    Scaler$Mean = Avg;
    Scaler$Std = Std;
    return(Scaler);
}
NormalScaler.Transform = function(x, scaler)
{
    Scaled = (x - scaler$Mean) / scaler$Std;
}
NormalScaler.Inverse = function(x, scaler)
{
    Scaled = x * scaler$Std + scaler$Mean;
}

RobustScaler = function(x, quantile.range = c(0.25, 0.75))
{
    MinValue = quantile(x, quantile.range[1]);
    MaxValue = quantile(x, quantile.range[2]);
    Scaler = list();
    Scaler$Min = MinValue;
    Scaler$Max = MaxValue;
    Scaler$Range = MaxValue - MinValue;
    Scaler$QuantileRange = quantile.range;
    return(Scaler);
}
RobustScaler.Transform = function(x, scaler)
{
    Scaled = (x - scaler$Min) / scaler$Range;
}
RobustScaler.Inverse = function(x, scaler)
{
    Scaled = x * scaler$Range + scaler$Min;
}

#A = c(1:30);

#A.MinMaxScaler = MinMaxScaler(A, range = c(-2, 2));
#A.Transform = MinMaxScaler.Transform(A, A.MinMaxScaler);
#A.Inverse = MinMaxScaler.Inverse(A.Transform, A.MinMaxScaler);
#print(A.Transform);
#print(A.Inverse);