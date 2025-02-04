/*
MatrixAlgebra.sql
Copyright 2013-2016 by Ernst Henle

This code was developed in SQLServer Management Studio
There are some differences among t-SQL, MySQL, SQLfiddle, sqlite, etc.
*/

USE
MatrixAlgebra
GO

/*

Sparse 2D matrices 
In these exercises numerical matrices are encoded using the sparse matrix format.

*/


BEGIN TRY 
DROP TABLE Matrix1
END TRY BEGIN CATCH END CATCH
BEGIN TRY 
DROP TABLE Matrix2
END TRY BEGIN CATCH END CATCH
BEGIN TRY 
DROP TABLE Matrix3
END TRY BEGIN CATCH END CATCH

/*Create sparse 2D matrices*/ 
CREATE TABLE Matrix1 (RowID nchar(10), ColumnID nchar(10), Value real); 
CREATE TABLE Matrix2 (RowID nchar(10), ColumnID nchar(10), Value real); 
CREATE TABLE Matrix3 (RowID nchar(10), ColumnID nchar(10), Value real); 

/*Load values into Matrix1*/ 
INSERT INTO Matrix1 VALUES ('one', 'one', -2)
INSERT INTO Matrix1 VALUES ('one', '2', 2)
INSERT INTO Matrix1 VALUES ('one', '3', -1)
INSERT INTO Matrix1 VALUES ('one', '4', 1)

-- Matrix1 is in the sparse matrix format and represents this 1X4 matrix:
----------------------------------
--      |'one'| '2' | '3' | '4' |
----------------------------------
-- 'one'| -2  |  2  |  -1 |  1  |
----------------------------------
SELECT * FROM Matrix1

INSERT INTO Matrix2 VALUES ('one', 'one', 0)
INSERT INTO Matrix2 VALUES ('2', 'one', 2)
INSERT INTO Matrix2 VALUES ('3', 'one', 3)
INSERT INTO Matrix2 VALUES ('4', 'one', 5)

-- Matrix2 is in the sparse matrix format and represents this 4X1 matrix:
---------------
--      |'one'|
---------------
-- 'one'|  0  |
--  '2' |  2  |
--  '3' |  3  |
--  '4' |  5  |
---------------
SELECT * FROM Matrix2

-- Exercise 1a
-- Multiply a 1X4 matrix with a 4X1 matrix
-- to create a 1X1 matrix (1 element)
-- Matrix1 X Matrix2
---------------
--      |'one'|
---------------
-- 'one'|  6  |
---------------
-- Answer for Exercise 1a:
SELECT Matrix1.RowID AS RowID, Matrix2.ColumnID AS ColumnID, SUM(Matrix1.Value * Matrix2.Value) AS Value 
    FROM Matrix1, Matrix2 WHERE Matrix1.ColumnID = Matrix2.RowID 
    GROUP BY Matrix1.RowID, Matrix2.ColumnID; 

-- Exercise 1b
-- In the above multiplication, switch occurrence of Matrix1 with Matrix2
-- The result is:  Matrix2 X Matrix1
----------------------------------
--      |'one'| '2' | '3' | '4' |
----------------------------------
-- 'one'|  0  |  0  |  0  |  0  |
--  '2' | -4  |  4  | -2  |  2  |
--  '3' | -6  |  6  | -3  |  3  |
--  '4' | -10 | 10  | -5  |  5  |
----------------------------------
-- Answer for Exercise 1b:
SELECT Matrix2.RowID AS RowID, Matrix1.ColumnID AS ColumnID, SUM(Matrix2.Value * Matrix1.Value) AS Value 
    FROM Matrix2, Matrix1 WHERE Matrix2.ColumnID = Matrix1.RowID 
    GROUP BY Matrix2.RowID, Matrix1.ColumnID; 

-- Exercise 2
-- Repeat Exercise 1a with transposed matrices
-- Matrix1 is transposed from a 1X4 matrix to a 4X1 matrix
-- Matrix2 is transposed from a 4X1 matrix to a 1X4 matrix
-- It is not necessary to drop the table for these schema changes!

TRUNCATE TABLE Matrix1
INSERT INTO Matrix1 VALUES ('one', 'one', -2)
INSERT INTO Matrix1 VALUES ('2', 'one', 2)
INSERT INTO Matrix1 VALUES ('3', 'one', -1)
INSERT INTO Matrix1 VALUES ('4', 'one', 1)

-- Matrix1 represents a 4X1 matrix
SELECT * FROM Matrix1

TRUNCATE TABLE Matrix2
INSERT INTO Matrix2 VALUES ('one', 'one', 0)
INSERT INTO Matrix2 VALUES ('one', '2', 2)
INSERT INTO Matrix2 VALUES ('one', '3', 3)
INSERT INTO Matrix2 VALUES ('one', '4', 5)
SELECT * FROM Matrix2

-- Repeat the same SQL as in Exercise 1a but this time:
-- Multiply a 4X1 matrix with a 1X4 matrix
-- to create a 4X4 matrix (16 elements)
-- Answer for Exercise 2:
SELECT Matrix1.RowID AS RowID, Matrix2.ColumnID AS ColumnID, SUM(Matrix1.Value * Matrix2.Value) AS Value 
    FROM Matrix1, Matrix2 WHERE Matrix1.ColumnID = Matrix2.RowID 
    GROUP BY Matrix1.RowID, Matrix2.ColumnID; 

-- Exercise 3
-- Repeat Exercise 1a with an incomplete matrix
-- Matrix1 is same as in Exercise 1
-- Matrix2 is similar to Exercise 2 except that some values are missing
-- Missing values assume a default value.  In this case the default value is zero (sort of like MATLAB)

TRUNCATE TABLE Matrix1
INSERT INTO Matrix1 VALUES ('one', 'one', -2)
INSERT INTO Matrix1 VALUES ('2', 'one', 2)
INSERT INTO Matrix1 VALUES ('3', 'one', -1)
INSERT INTO Matrix1 VALUES ('F', 'one', 1)
SELECT * FROM Matrix1

TRUNCATE TABLE Matrix2
INSERT INTO Matrix2 VALUES ('one', 'one', 0)
--INSERT INTO Matrix2 VALUES ('one', '2', 2)
INSERT INTO Matrix2 VALUES ('one', '3', 3)
--INSERT INTO Matrix2 VALUES ('one', 'F', 5)
SELECT * FROM Matrix2

-- Answer for Exercise 3:
SELECT Matrix1.RowID AS RowID, Matrix2.ColumnID AS ColumnID, SUM(Matrix1.Value * Matrix2.Value) AS Value 
    FROM Matrix1, Matrix2 WHERE Matrix1.ColumnID = Matrix2.RowID 
    GROUP BY Matrix1.RowID, Matrix2.ColumnID; 

-- Exercise 4
-- Matrix multiplication with same code as in Exercises 1, 2, & 3
-- But, with a 2-D matrix as opposed to 1-D matrices

-- Matrix1 is a 4 by 3 matrix
TRUNCATE TABLE Matrix1
INSERT INTO Matrix1 VALUES ('1', '1', -2)
INSERT INTO Matrix1 VALUES ('2', '1', 2)
INSERT INTO Matrix1 VALUES ('3', '1', -1)
INSERT INTO Matrix1 VALUES ('4', '1', 1)
INSERT INTO Matrix1 VALUES ('1', '2', 2)
INSERT INTO Matrix1 VALUES ('2', '2', -2)
INSERT INTO Matrix1 VALUES ('3', '2', 1)
INSERT INTO Matrix1 VALUES ('4', '2', -1)
INSERT INTO Matrix1 VALUES ('1', '3', 0)
INSERT INTO Matrix1 VALUES ('2', '3', -1)
INSERT INTO Matrix1 VALUES ('3', '3', 0)
INSERT INTO Matrix1 VALUES ('4', '3', 1)
---------------------------
--      | '1' | '2' | '3' |
---------------------------
--  '1' | -2  |  2  |  0  |
--  '2' |  2  | -2  | -1  |
--  '3' | -1  |  1  |  0  |
--  '4' |  1  | -1  |  1  |
---------------------------
SELECT * FROM Matrix1

-- Matrix2 is 3 by 4 matrix
TRUNCATE TABLE Matrix2
INSERT INTO Matrix2 VALUES ('1', '1', 0)
INSERT INTO Matrix2 VALUES ('1', '2', 2)
INSERT INTO Matrix2 VALUES ('1', '3', 4)
INSERT INTO Matrix2 VALUES ('1', '4', 6)
INSERT INTO Matrix2 VALUES ('2', '1', 1)
INSERT INTO Matrix2 VALUES ('2', '2', 3)
INSERT INTO Matrix2 VALUES ('2', '3', 5)
INSERT INTO Matrix2 VALUES ('2', '4', 7)
INSERT INTO Matrix2 VALUES ('3', '1', -1)
INSERT INTO Matrix2 VALUES ('3', '2', 1)
INSERT INTO Matrix2 VALUES ('3', '3', 3)
INSERT INTO Matrix2 VALUES ('3', '4', 5)
---------------------------------
--      | '1' | '2' | '3' | '4' |
---------------------------------
--  '1' |  0  |  2  |  4  |  6  |
--  '2' |  1  |  3  |  5  |  7  |
--  '3' | -1  |  1  |  3  |  5  |
---------------------------------
SELECT * FROM Matrix2

-- Exercise 4a
-- Multiply Matrix1 with Matrix2 to create a 4 by 4 matrix
-- Matrix1 X Matrix2 is this 4 by 4 matrix:
---------------------------------
--      | '1' | '2' | '3' | '4' |
---------------------------------
--  '1' |  2  |  2  |  2  |  2  |
--  '2' | -1  | -3  | -5  | -7  |
--  '3' |  1  |  1  |  1  |  1  |
--  '4' | -2  |  0  |  2  |  4  |
---------------------------------
-- Answer for Exercise 4a:
SELECT Matrix1.RowID AS RowID, Matrix2.ColumnID AS ColumnID, SUM(Matrix1.Value * Matrix2.Value) AS Value 
    FROM Matrix1, Matrix2 WHERE Matrix1.ColumnID = Matrix2.RowID 
    GROUP BY Matrix1.RowID, Matrix2.ColumnID;

-- Exercise 4b
-- Multiply Matrix2 with Matrix1 to create a 4 by 4 matrix
-- Matrix2 X Matrix1 is this 3 by 3 matrix:
---------------------------
--      | '1' | '2' | '3' |
---------------------------
--  '1' |  6  | -6  |  4  |
--  '2' |  6  | -6  |  4  |
--  '3' |  6  | -6  |  4  |
---------------------------
-- Answer for Exercise 4b:
SELECT Matrix2.RowID AS RowID, Matrix1.ColumnID AS ColumnID, SUM(Matrix2.Value * Matrix1.Value) AS Value 
    FROM Matrix2, Matrix1 WHERE Matrix2.ColumnID = Matrix1.RowID 
    GROUP BY Matrix2.RowID, Matrix1.ColumnID;

--Exercise 5
-- Write SQL to multiply a matrix in the sparse matrix format by 7.  Use Matrix1 as your example matrix.
-- The result of the multiplication is a matrix in the sparse matrix format.
-- Do not use insert statements or create any new tables or views.
-- Answer for Exercise 5:
SELECT RowID, ColumnID, 7*Value AS Value FROM Matrix1
-- The result is an EAV that represents this matrix:
---------------------------
--      | '1' | '2' | '3' |
---------------------------
--  '1' |-14  | 14  |  0  |
--  '2' | 14  |-14  | -7  |
--  '3' | -7  |  7  |  0  |
--  '4' |  7  | -7  |  7  |
---------------------------

--Exercise 6
-- Write SQL to transpose a matrix in the sparse matrix format.  Use Matrix2 as your example matrix.
-- The result of the transposition will be a matrix in the sparse matrix format.
-- Do not use insert statements or create any new tables or views.
-- Answer for Exercise 6:
SELECT ColumnID AS RowID, RowID AS ColumnID, Value FROM Matrix2
-- The result is an EAV that represents this matrix:
---------------------------
--      | '1' | '2' | '3' |
---------------------------
--  '1' |  0  |  1  | -1  |
--  '2' |  2  |  3  |  1  |
--  '3' |  4  |  5  |  3  |
--  '4' |  6  |  7  |  5  |
---------------------------

--Exercise 7
-- Add two Matrices
-- Matrix3 represents a 4 by 3 matrix
TRUNCATE TABLE Matrix3
INSERT INTO Matrix3 VALUES ('1', '1', 10)
INSERT INTO Matrix3 VALUES ('2', '1', 20)
INSERT INTO Matrix3 VALUES ('3', '1', 30)
INSERT INTO Matrix3 VALUES ('4', '1', 40)
INSERT INTO Matrix3 VALUES ('1', '2', 50)
INSERT INTO Matrix3 VALUES ('2', '2', 60)
INSERT INTO Matrix3 VALUES ('3', '2', 70)
INSERT INTO Matrix3 VALUES ('4', '2', 80)
INSERT INTO Matrix3 VALUES ('1', '3', 90)
INSERT INTO Matrix3 VALUES ('2', '3', 100)
INSERT INTO Matrix3 VALUES ('3', '3', 110)
INSERT INTO Matrix3 VALUES ('4', '3', 120)
SELECT * FROM Matrix3
-- Write SQL to create a sparse matrix representation that is the addition of the matrices represented by Matrix1 and Matrix3
-- The result is in the sparse matrix format.
-- Do not use insert statements or create any new tables or views.
-- Answer for Exercise 7:
SELECT Matrix1.RowID AS RowID, Matrix1.ColumnID AS ColumnID, (ISNULL(Matrix3.Value, 0) + ISNULL(Matrix1.Value, 0)) AS Value 
    FROM Matrix1 FULL OUTER JOIN Matrix3 ON (Matrix1.ColumnID = Matrix3.ColumnID) AND (Matrix1.RowID = Matrix3.RowID)
----------------------------------