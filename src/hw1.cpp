#include<iostream>
#include<vector>
#include<iomanip>
#include<random>
#include<exception>
#include<stdexcept>
#include"hw1.h"



namespace algebra

{

using Matrix = std::vector<std::vector<double>>;

Matrix zeros(size_t n, size_t m)
{   
    if( m < 1 || n < 1)
        {
            std::logic_error e{"inputs must be bigger than one"};
            throw e;
        }
    Matrix output(n, std::vector<double> (m, 0.0));
    return output;
}

 Matrix ones (size_t n, size_t m)
{   
    if( m < 1 || n < 1)
        {
            std::logic_error e{"inputs must be bigger than one"};
            throw e;
        }
    Matrix output(n, std::vector<double> (m, 1.0));
    return output;
}

void show(const Matrix& matrix)
{   
    if( matrix.empty())
        {
            std::logic_error e{"matrix is empty"};
            throw e;
        }
    for(size_t i{}; i < matrix.size() ; i++)
        {   
            std::cout << "|";
            for(size_t j{}; j < matrix[i].size() ; j++)
                std::cout << std::setprecision(3) << std::setw(8) << matrix[i][j] ;
            std::cout << std::setw(8) << "|" << std::endl;
        }
}

Matrix random(size_t n, size_t m, double min, double max)
{ 
    if( m < 1 || n < 1 )
    {
        std::logic_error e{"zero row or columns"};
        throw e;
    }
    if(min > max)
    {
        std::logic_error e{"max must be bigger than min"};
        throw e;
    }
    Matrix output{zeros(n,m)};
    for(size_t i{}; i < n; i++)
    {
    for(size_t j{}; j < m; j++)
        {
            std::random_device rd;
            std::mt19937 mt(rd());
            std::uniform_real_distribution<double> dist(min, max);
            output[i][j]  = dist(mt);
        }
    }
    return output;
}

Matrix multiply(const Matrix& matrix, double c)
{   
    if( matrix.empty())
        {
            std::logic_error e{"matrix is empty"};
            throw e;
        }
    size_t n{ matrix.size() }, m{ matrix[0].size() };
    Matrix output{zeros(n,m)};
    for(size_t i{}; i < n; i++)
    {
        for(size_t j{}; j < m; j++)
        {
            output[i][j] = matrix[i][j] * c;
        }
    }
    return output;
}

Matrix multiply(const Matrix& matrix1, const Matrix& matrix2)
{      
    if(matrix1.empty() || matrix2.empty())
    {   
        return matrix1;
    }
    size_t r1{ matrix1.size() }, c1{ matrix1[0].size() };
    size_t r2{ matrix2.size() }, c2{ matrix2[0].size() };
    if(c1 != r2)
    {
        std::logic_error e{"c1 must be equal to r2"};
        throw e;
    }
    Matrix output{zeros(r1,c2)};
    for(size_t i{}; i < r1; i++)
    {
        for(size_t j{}; j < c2; j++)
        {   
            double sum{};
            for(size_t k{}; k < c1; k++)
            {
                sum += matrix1[i][k]*matrix2[k][ j ];
            }
            output[i][j] = sum;
        }
    }
    return output;
}
    


Matrix sum(const Matrix& matrix, double c)
{
    if( matrix.empty())
    {
        return matrix;
    }
    size_t n{ matrix.size() }, m{ matrix[0].size() };
    Matrix output{zeros(n,m)};
    for(size_t i{}; i < n; i++)
    {
        for(size_t j{}; j < m; j++)
            {
                output[i][j] = matrix[i][j] + c;
            }
    }
    return output;
}

Matrix sum(const Matrix& matrix1, const Matrix& matrix2)
{
    
    if(matrix1.empty() && matrix2.empty())
    {
        return matrix1;
    }
    if(matrix1.empty() || matrix2.empty())
    {
        std::logic_error e{"matrixs must have same dementions"};
        throw (e);
    }
    size_t r1{ matrix1.size() }, c1{ matrix1[0].size() };
    size_t r2{ matrix2.size() }, c2{ matrix2[0].size() };
    Matrix output {zeros(r1,c1)};
    if(r1 != r2 || c1 != c2)
    {
        std::logic_error e{"matrixs must have same dementions"};
        throw (e);
    }
    for(size_t i{}; i < r1; i++)
    {
        for(size_t j{}; j < c1; j++)
        {
            output[i][j] = matrix1[i][j] + matrix2[i][j];
        }
    }
    return output;
}

Matrix transpose(const Matrix& matrix)
{   
    if( matrix.empty())
    {
        return matrix;
    }
    size_t r{ matrix.size() }, c{ matrix[0].size() };
    Matrix output{zeros(c,r)};
    for(size_t i{}; i < r; i++)
    {
        for(size_t j{}; j < c; j++)
        {
            output[j][i] = matrix[i][j];
        }
    }
    return output;
}

Matrix minor(const Matrix& matrix, size_t n, size_t m)
{   
    size_t r{ matrix.size() }, c{ matrix[0].size() };
    Matrix output{zeros(r-1, c-1)};
    size_t i_minor{}, j_minor{};    
    if(matrix.empty())
    {
        std::logic_error e{"matrix is empty"};
        throw e;
    }
    if(n > r ||  m > c)
    {
        std::logic_error e{"out rage row or colmun"};
        throw e;
    }
    for(size_t i{}; i < r; i++)
    {
        if( i == n)
            continue;
        for(size_t j{}; j < c; j++)
        {
            if( j == m)
                continue;
            output[i_minor][j_minor] = matrix[i][j];
            if(!(j_minor == c - 2))
                j_minor++;
            else
            {
                i_minor++;
                j_minor = 0;
            }
        }
    }
    return output;  
}

double determinant(const Matrix& matrix)
{   
    if(matrix.empty())
    {
        return 1;
    }
    size_t r{ matrix.size() }, c{ matrix[0].size() };
    double sum{};
    if(r != c)
    {
        std::logic_error e{"rows and colmuns are not equal"};
        throw e;
    }
    if(r == 1 && c == 1)
        return matrix[0][0];
    for(size_t i{}; i < r; i++)
    {
        sum += matrix[i][0]*pow(-1, i) * determinant(minor(matrix,i,0));
    }       
    return sum;
}

Matrix inverse(const Matrix& matrix)
{
    
    if(matrix.empty())
    {
        return matrix;
    }
    size_t r{ matrix.size() }, c{ matrix[0].size() };
    Matrix handi_matrix{zeros(r, c)};
    if(r != c)
    {
        std::logic_error e{"rows and colmuns are not equal"};
        throw e;
    }
    if(determinant(matrix) == 0.0)
    {
        std::logic_error e{"determinant is zero"};
        throw e;
    }
    for(size_t i{}; i < r; i++)
    {
        for(size_t j{}; j < c; j++)
        {
            handi_matrix[i][j] = pow(-1, i + j) * determinant(minor(matrix, i, j));
        }
    }
    return multiply(transpose(handi_matrix), 1/determinant(matrix));
}

Matrix concatenate(const Matrix& matrix1, const Matrix& matrix2, int axis=0)
{
    if(matrix1.empty() || matrix2.empty())
    {
        std::logic_error e{"matrix1 or matrix2 is empty"};
        throw e;
    }
    size_t r1{ matrix1.size() }, c1{ matrix1[0].size() };
    size_t r2{ matrix2.size() }, c2{ matrix2[0].size() };
    if(axis == 0 && c1 != c2)
    {
        std::logic_error e{"for axis == 0 columns must be same size"};
        throw e;
    }
    if(axis ==1 && r1 != r2)
    {
        std::logic_error e{"for axis == 1 rows must be same size"};
        throw e;
    }
    if(axis == 0)
    {   
        Matrix output(r1 + r2, std::vector<double>(c1, 0.0));
        for(size_t i{}; i < r1 ; i++)
        {
            for(size_t j{}; j < c1; j++)
            {
                output[i][j] = matrix1[i][j];
            }
        }     
        for(size_t i{ r1 }; i < r1 + r2; i++)
        {
            for(size_t j{}; j < c1; j++)
            {
                output[i][j] = matrix2[i - r1][j];
            }
        }  
        return output;
    }
    else
        {
            Matrix output(r1 , std::vector<double>(c1 + c2, 0.0));
            for(size_t i{}; i < r1 ; i++)
            {
                for(size_t j{}; j < c1; j++)
                {
                    output[i][j] = matrix1[i][j];
                }
            }     
            for(size_t i{}; i < r1 ; i++)
            {
                for(size_t j{c1}; j < c1 + c2; j++)
                {
                    output[i][j] = matrix2[i][j - c1];
                }
            }   
            return output;
        }
}

Matrix ero_swap(const Matrix& matrix, size_t r1, size_t r2)
{  
    size_t n{matrix.size()};
    if(matrix.empty())
    {
        std::logic_error e{"matrix is empty"};
        throw e;
    }
    if(r1 >= n || r2 >= n)
    {
        std::logic_error e{"out rage rows"};
        throw e;
    }
    Matrix output{matrix};
    std::vector<double> temp{ output[ r1]};
    output[r1] = output[r2];
    output[r2] = temp;
    return output;

}
 
Matrix ero_multiply(const Matrix& matrix, size_t r, double c)
{  
    size_t n{matrix.size()};
    if(matrix.empty())
    {
        std::logic_error e{"matrix is empty"};
        throw e;
    }
    if(r >= n)
    {
        std::logic_error e{"out rage row"};
        throw e;
    }
    Matrix output{matrix};
    output[r] =  multiply(matrix, c)[r];
    return output;
}

Matrix ero_sum(const Matrix& matrix, size_t r1, double c, size_t r2)
{  
    size_t n{matrix.size()};
    if(matrix.empty())
    {
        std::logic_error e{"matrix is empty"};
        throw e;
    }
    if(r1 >= n || r2 >= n)
    {
        std::logic_error e{"out rage rows"};
        throw e;
    }
    Matrix output{matrix};
    size_t m{ matrix[0].size() };
    std::vector<double> temp(m, 0);
    for(size_t i{}; i < m; i++)
    {
        temp[i] = matrix[r1][i] * c + matrix[r2][i];
    }
    output[r2] = temp;
    return output;
}

Matrix upper_triangular(const Matrix& matrix)
{  
    if(matrix.empty())
    {
        return matrix;
    }
    size_t r{ matrix.size() }, c{ matrix[0].size() };
    if(r != c)
    {
        std::logic_error e{"rows and colmuns are not equal"};
        throw e;
    }
    Matrix output{matrix};
    for(size_t i{}; i < r-1; i++)
    {
        if(output[i][i] == 0 )
        {
            for(size_t j{i+1}; j < r; j++)
            {
                if(output[j][i] != 0)
                {
                    output = ero_swap(output,i,j);
                    break;
                }
            }
        }
            
    }        
    for(size_t i{}; i < r - 1; i++)
    {
        for(size_t j{i+1}; j < r; j++)
        {
            output = (ero_sum(output, i, - 1* output[j][i] / output[i][i], j));
        }
    }
    return output;
}

}
