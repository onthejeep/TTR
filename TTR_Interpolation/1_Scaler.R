

MinMaxScaler.Transform = function(input, scaler)
{
    Scaled = (input - scaler$Min) / scaler$Range;
}

MinMaxScaler = function(input)
{
    MinValue = min(input);
    MaxValue = max(input);

    Scaler = list();
    Scaler$Min = MinValue;
    Scaler$Max = MaxValue;
    Scaler$Range = MaxValue - MinValue;
    return(Scaler);
}

MinMaxScaler.Inverse = function(input, scaler)
{
    Scaled = input * scaler$Range + scaler$Min;
}

NormalScaler = function(input)
{
    Avg = mean(input);
    Std = sd(input);
    Scaler = list();
    Scaler$Mean = Avg;
    Scaler$Std = Std;
    return(Scaler);
}
NormalScaler.Transform = function(input, scaler)
{
    Scaled = (input - scaler$Mean) / scaler$Std;
}
NormalScaler.Inverse = function(input, scaler)
{
    Scaled = input * scaler$Std + scaler$Mean;
}

RobustScaler = function(input, quantile.range = c(0.25, 0.75))
{
    MinValue = quantile(input, quantile.range[1]);
    MaxValue = quantile(input, quantile.range[2]);
    Scaler = list();
    Scaler$Min = MinValue;
    Scaler$Max = MaxValue;
    Scaler$Range = MaxValue - MinValue;
    Scaler$QuantileRange = quantile.range;
    return(Scaler);
}
RobustScaler.Transform = function(input, scaler)
{
    Scaled = (input - scaler$Min) / scaler$Range;
}
RobustScaler.Inverse = function(input, scaler)
{
    Scaled = input * scaler$Range + scaler$Min;
}

#A = c(-99, 1:30, 99);

#A.MinMaxScaler = RobustScaler(A);
#A.Transform = RobustScaler.Transform(A, A.MinMaxScaler);
#A.Inverse = RobustScaler.Inverse(A.Transform, A.MinMaxScaler);
#print(A.Transform);
#print(A.Inverse);