
### DP Results


#### Final policy

\begin{array}{|c|c|c|c|c|c|c|c|c|c|}
\hline
\downarrow  & \downarrow  & \downarrow  & \searrow  & \searrow  & \searrow  & \rightarrow  & \rightarrow  & \searrow  & \downarrow  \\\\
\hline
\downarrow  & \downarrow  & \downarrow  & \swarrow  & \searrow  & \searrow  & \searrow  & \searrow  & \searrow  & \downarrow  \\\\
\hline
\searrow  & \downarrow  & \downarrow  & \searrow  & \searrow  & \searrow  & \searrow  & \searrow  & \swarrow  & \downarrow  \\\\
\hline
S & \searrow  & \downarrow  & \searrow  & \searrow  & \searrow  & \searrow  & G & \swarrow  & \swarrow  \\\\
\hline
\rightarrow  & \searrow  & \searrow  & \searrow  & \searrow  & \searrow  & \searrow  & \downarrow  & \swarrow  & \leftarrow  \\\\
\hline
\rightarrow  & \rightarrow  & \searrow  & \searrow  & \searrow  & \searrow  & \searrow  & \downarrow  & \leftarrow  & \leftarrow  \\\\
\hline
\rightarrow  & \rightarrow  & \rightarrow  & \searrow  & \searrow  & \searrow  & \rightarrow  & \cdot  & \nwarrow  & \leftarrow  \\\\
\hline
\end{array}

#### Final value function

\begin{array}{|c|c|c|c|c|c|c|c|c|c|}
\hline
-12.5 & -12.5 & -12.5 & -13.1 & -12.2 & -11.3 & -10.3 & -9.3 & -8.3 & -7.6 \\\\
\hline
-11.5 & -11.5 & -11.5 & -12.5 & -11.8 & -11.1 & -10.2 & -9.1 & -7.6 & -6.6 \\\\
\hline
-10.5 & -10.5 & -10.5 & -11.5 & -10.7 & -9.9 & -9.9 & -8.5 & -6.5 & -5.6 \\\\
\hline
S & -9.5 & -9.5 & -10.1 & -9.1 & -8.2 & -6.5 & G & -5.2 & -4.6 \\\\
\hline
-10.0 & -9.0 & -8.5 & -8.6 & -7.5 & -6.1 & -5.2 & -5.2 & -3.6 & -4.6 \\\\
\hline
-10.0 & -9.0 & -8.0 & -7.5 & -6.3 & -5.2 & -3.6 & -3.6 & -3.6 & -4.6 \\\\
\hline
-10.0 & -9.0 & -8.0 & -7.0 & -5.8 & -4.6 & -3.6 & -3.6 & -3.6 & -4.6 \\\\
\hline
\end{array}

#### Trajectory of final policy

\begin{array}{|c|c|c|c|c|c|c|c|c|c|}
\hline
 &  &  &  &  &  &  &  &  &  \\\\
\hline
 &  &  &  &  &  &  &  &  &  \\\\
\hline
 &  &  &  &  &  &  &  &  &  \\\\
\hline
S &  &  &  &  &  &  & G &  &  \\\\
\hline
 &  \cdot  &  &  &  &  &  &  &  &  \\\\
\hline
 &  &  \cdot  &  &  &  &  &  \cdot  &  &  \\\\
\hline
 &  &  &  \cdot  &  \cdot  &  \cdot  &  \cdot  &  &  &  \\\\
\hline
\end{array}

#### Policy/Value Function Changes vs. Iteration


![](img/valueDiffs.png)

### TD Results

epsilon = 0.10  
beta = 0.000000  

#### Mean Square Value Function Error vs. DP


![](img/vFuncErr.png)  
circle: alpha=0.1  
plus: alpha=0.2  
line: alpha=0.5  
