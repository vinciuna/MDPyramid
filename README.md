## MDPyramid Doc

Simple MDPyramid script in R. 
Delivers 2 possible routes with maximum sums.

The task to find the maximum sum of the numbers per the given rules below:  

1. You will start from the top and move downwards to an adjacent number as in below.  
2. You are only allowed to walk downwards and diagonally.  
3. You should walk over the numbers as evens and odds subsequently. Suppose that you are on an even number the next number you walk must be odd, or if you are stepping over an odd number the next number must be even. In other words, the final path would be like Odd -> even -> odd -> even …  
4. You must reach to the bottom of the pyramid.  

The goal is to find the maximum sum if you walk the path. 

Pros:
* This is v01- simple approach, without recursive functions. Code loops over the lines and looking for best 2 sums.  
* As it track for maximum 2 possible routes with maximum sums, it doesn`t encouters if boths routes will drop [face not allowed jumps] due merged way.
