N = 100
a = sample(1:100, 100, replace=T)
b = sample(1:100, 100, replace=T)
A = matrix(nrow = 100, ncol = 100)
B = matrix(nrow = 100, ncol = 100)
sumA = c()
probA = c()
error = c()
errorT=c()
upbound = c()

for (i in 1:N)
{
	A[i,] = sample(1:a[i], 100, replace=T)
	B[,i] = sample(1:b[i], 100, replace=T)
	sumA[i] = sum(A[,i]^2)
	probA[i] = sumA[i]/sum(sumA) 
}		
for (i in 1:3)
{
	for(k in 1:100)
	{
		colAsp = sample(1:100, i, replace=T, prob=probA)
		Asp = matrix(nrow=100,ncol=i)
		Bsp = matrix(nrow=i,ncol=100)
		for (j in 1:i)
		{
			Asp[,j] = A[,colAsp[j]]/sqrt(i*probA[colAsp[j]])
			Bsp[j,] = B[colAsp[j],]/sqrt(i*probA[colAsp[j]])
		}
		errorT[k] = norm(A%*%B-Asp%*%Bsp,type="2")
		error[i] = mean(errorT)
	}
}
		 
		
	
