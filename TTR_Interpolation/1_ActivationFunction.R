

Activation.Identity = function(x)
{ # (-inf, inf)
    return(x);
}

Activation.Binary = function(x, threshold)
{ # {0, 1}
    ifelse(x < threshold, return(0), return(1));
}

Activation.Logistic = function(x)
{ # (0, 1)
    return(1 / (1 + exp(-x)));
}

Activation.TanH = function(x)
{ # (-1, 1)
    return(tanh(x));
}

Activation.Softsign = function(x)
{ # (-1, 1)
    return(x / (1 + abs(x)));
}

Threshold.Velocity = function(x, threshold)
{
    if (x < -threshold)
    {
        return(-threshold);
    }
    else if (x > threshold)
    {
        return(threshold);
    }
    else
    {
        return(x);
    }
}

