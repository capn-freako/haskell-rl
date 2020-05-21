
### Final policy

\begin{array}{
c|c|c|c|c|c
}
\text{On Hand} & & & & & &  \\
\hline
10 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
9 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
8 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
7 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
6 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
5 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
4 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
3 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
2 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
1 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
0 & 5 & 5 & 5 & 5 & 5 & 5 \\
\hline
\text{On Order:}  & 0 & 1 & 2 & 3 & 4 & 5
\end{array}

### Final value function

\begin{array}{
c|c|c|c|c|c
}
\text{On Hand} & & & & & &  \\
\hline
10 & -92.0 & -79.6 & -68.9 & -60.3 & -54.1 & -50.2 \\
\hline
9 & -104.6 & -92.0 & -79.6 & -68.9 & -60.3 & -54.1 \\
\hline
8 & -116.6 & -104.6 & -92.0 & -79.6 & -68.9 & -60.3 \\
\hline
7 & -127.4 & -116.6 & -104.6 & -92.0 & -79.6 & -68.9 \\
\hline
6 & -136.8 & -127.4 & -116.6 & -104.6 & -92.0 & -79.6 \\
\hline
5 & -144.4 & -136.8 & -127.4 & -116.6 & -104.6 & -92.0 \\
\hline
4 & -150.1 & -144.4 & -136.8 & -127.4 & -116.6 & -104.6 \\
\hline
3 & -154.0 & -150.1 & -144.4 & -136.8 & -127.4 & -116.6 \\
\hline
2 & -156.3 & -154.0 & -150.1 & -144.4 & -136.8 & -127.4 \\
\hline
1 & -157.5 & -156.3 & -154.0 & -150.1 & -144.4 & -136.8 \\
\hline
0 & -157.7 & -157.5 & -156.3 & -154.0 & -150.1 & -144.4 \\
\hline
\text{On Order:}  & 0 & 1 & 2 & 3 & 4 & 5
\end{array}

![](img/valueDiffs_inv.png)

### TD Results

epsilon = 0.10  
beta = 0.000000  

#### Mean Square Value Function Error vs. DP


![](img/vFuncErr_inv.png)  
circle: alpha=0.1  
plus: alpha=0.2  
line: alpha=0.5  

## debug


### Demand Probability Distribution Functions

![](img/pdf.png)

$\int pdf =  1.00$

![](img/demand.png)

$\sum pmf =  1.00$

Next state PMF sums: min =  1.00; max =  1.00.
