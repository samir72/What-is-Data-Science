-----------------------------------------------------------------------
/*
RelationalAlgebraSQLHomework.sql
Copyright 2013-2015 by Ernst Henle

This code was developed in SQLServer Management Studio
There are some differences in MySQL and SQL Fiddle:  http://sqlfiddle.com/
*/
-----------------------------------------------------------------------

-----------------------------------------------------------------------
-- 1
-----------------------------------------------------------------------
--{a, b, c} is a relation that contains the tuples a, b, and c.
-- In the following cases the tuples have arity of 1. 
-- Calculate the following:
-- 1a
-- ({1, 2, 3} ᴜ {5, 7, 11}) ∩ {2, 4, 6, 8, 10}
-- 1b
--({1, 2, 3} ∩ {2, 4, 6, 8, 10}) ᴜ ({5, 7, 11} ∩ {2, 4, 6, 8, 10})
USE
MatrixAlgebra
GO
DROP TABLE R
GO
CREATE TABLE R
(
C1 int,
)
GO
INSERT INTO R VALUES (1)
INSERT INTO R VALUES (2)
INSERT INTO R VALUES (3)
GO
DROP TABLE S
GO
CREATE TABLE S
(
C1 int,
)
GO
INSERT INTO S VALUES (5)
INSERT INTO S VALUES (7)
INSERT INTO S VALUES (11)
GO
DROP TABLE T
GO
CREATE TABLE T
(
C1 int,
)
GO
INSERT INTO T VALUES (2)
INSERT INTO T VALUES (4)
INSERT INTO T VALUES (6)
INSERT INTO T VALUES (8)
INSERT INTO T VALUES (10)
-- 1a
-- ({1, 2, 3} ᴜ {5, 7, 11}) ∩ {2, 4, 6, 8, 10}
-- {1, 2, 3} ᴜ {5, 7, 11} -> {1, 2, 3, 5, 7, 11}
-- {1, 2, 3, 5, 7, 11} ∩ {2, 4, 6, 8, 10} -> {2}
(SELECT * FROM R
UNION
SELECT * FROM S)
INTERSECT
SELECT * FROM T
--
-- 1b
-- ({1, 2, 3} ∩ {2, 4, 6, 8, 10}) ᴜ ({5, 7, 11} ∩ {2, 4, 6, 8, 10})
-- {1, 2, 3} ∩ {2, 4, 6, 8, 10} -> {2}
-- {5, 7, 11} ∩ {2, 4, 6, 8, 10} -> {}
-- {2} ᴜ {} -> {2}
((SELECT * FROM R) INTERSECT (SELECT * FROM T))
UNION
((SELECT * FROM S) INTERSECT (SELECT * FROM T))
-----------------------------------------------------------------------
-- 2
-----------------------------------------------------------------------
-- Use formal notation to write an algebraic example of the following SQL:
-- 2a.SELECT Column1, Column3 FROM MyTable WHERE Column2 = Column3
-- 2b.Reverse the order of projection and selection in the algebraic formulation
USE
MatrixAlgebra
GO
DROP TABLE MyTable
GO
CREATE TABLE MyTable
(
Column1 nchar(10),
Column2 nchar(10),
Column3 nchar(10),
)
GO
INSERT INTO MyTable VALUES (0, 1, 1)
INSERT INTO MyTable VALUES (1, 2, 3)
INSERT INTO MyTable VALUES (1, 3, 5)
INSERT INTO MyTable VALUES (2, 5, 7)
INSERT INTO MyTable VALUES (3, 7, 9)
INSERT INTO MyTable VALUES (3, 11, 11)
--
-- 2
-- Items in squiggly brackets {} are subscripted
SELECT Column1, Column3 FROM MyTable WHERE Column2 = Column3
-- 2a π{C1, C3}(σ{C2=C3}(MyTable)
SELECT Column1, Column3 FROM (SELECT * FROM MyTable WHERE Column2 = Column3) as R
-- 2b
-- Trick question:
-- σ{C2=C3}(π{C1, C3}(MyTable)) not possible because {C2=C3} is undefined without C2!
SELECT * FROM (SELECT Column1, Column3 FROM MyTable) AS R WHERE MyTable.Column2 = R.Column3 -- not possible
-----------------------------------------------------------------------
-- 3
-----------------------------------------------------------------------
-- Items in squiggly brackets {} are subscripted
-- π{c1, c2}(σϕ1(σϕ2(π{c1, c2, c3, c5}(R))))
-- Where
--		ϕ1: C1 = C5;
--		ϕ2: C5 = “Test”;
--		R: MyTable;
-- 3a.Write a SQL statement that declares the intent of the algebraic notation
-- 3b.Simply the algebraic statement
USE
MatrixAlgebra
GO
DROP TABLE R
GO
CREATE TABLE R
(
C1 nchar(10),
C2 int,
C3 int,
C4 int,
C5 nchar(10)
)
GO
INSERT INTO R VALUES ('A', 1, 1, 0, 'Test')
INSERT INTO R VALUES ('Test', 2, 3, 0, 'Control')
INSERT INTO R VALUES ('C', 3, 5, 0, 'Test')
INSERT INTO R VALUES ('Test', 5, 7, 0, 'Control')
INSERT INTO R VALUES ('E', 7, 9, 0, 'Test')
INSERT INTO R VALUES ('Test', 11, 11, 0, 'Control')
INSERT INTO R VALUES ('Test', 13, 13, 0, 'Test')
GO
SELECT * FROM R
-- 3a
-- π{c1, c2}(σϕ1(σϕ2(π{c1, c2, c3, c5}(R))))
-- /*1*/ π{c1, c2, c3, c5}(R) -> S
-- /*2*/ σϕ2(S) -> T
-- /*3*/ σϕ1(T) -> U
-- /*4*/ π{c1, c2}(U)
/*4*/SELECT U.C1, U.C2 FROM
(
	/*3*/SELECT * FROM
		(/*2*/SELECT * FROM
			(/*1*/SELECT R.C1, R.C2, R.C3, R.C5 FROM R/*1*/) 
		as S WHERE S.C5 = 'Test'/*2*/)
	as T WHERE T.C1 = T.C5 /*3*/
) as U /*4*/
-- 3b
-- π{c1, c2}(σϕ1(σϕ2(π{c1, c2, c3, c5}(R))))
-- ϕ1 Λ ϕ2 = ϕ = (C1 = C5) Λ (C5 = "Test") -> (C1 = "Test") Λ (C5 = "Test")
-- π{c1, c2}(π{c1, c2, c3, c5}(R)) -> π{c1, c2}(R)
-- π{c1, c2}(σϕ(R))
-- σϕ(π{c1, c2}(R))
SELECT C1, C2 FROM R WHERE (C1 = C5) AND (C5 = 'Test') 
-- Beware the following SQL statement is incorrect.
-- You might think that you can reduce (C1 = C5) AND (C5 = 'Test') to only (C1 = 'Test')
-- This would allow cases where C5 <> 'Test'
SELECT C1, C2 FROM R WHERE (C1 =  'Test')

-- Alternate answer:
-- πc1, c2(σc1=c5(R) ∩ σc5=’Test’(R))
SELECT C1, C2 FROM
(SELECT * FROM R WHERE C1=C5
INTERSECT
SELECT * FROM R WHERE C5='Test') AS X

-----------------------------------------------------------------------
-- 4
-----------------------------------------------------------------------
-- SELECT * FROM T1 JOIN T2 ON T1.C1 = T2.C1
-- 4a. Write out an equivalent in relational algebra using the join operator
-- 4b. Write out an equivalent in relational algebra without using the join operator
USE
MatrixAlgebra
GO
DROP TABLE T1
GO
CREATE TABLE T1
(
C1 nchar(50),
C2 int,
C5 nchar(50)
)
INSERT INTO T1 VALUES ('A', 1, 'Smear')
INSERT INTO T1 VALUES ('A', 2, 'Cell Sorter')
INSERT INTO T1 VALUES ('B', 3, 'Smear')
INSERT INTO T1 VALUES ('B', 4, 'Cell Sorter')
INSERT INTO T1 VALUES ('B', 5, 'Elisa')
INSERT INTO T1 VALUES ('C', 6, 'Smear')
INSERT INTO T1 VALUES ('C', 7, 'Elisa')
SELECT * FROM T1

DROP TABLE T2
GO
CREATE TABLE T2
(
C1 nchar(50),
C5 nchar(50)
)
GO
INSERT INTO T2 VALUES ('A', 'Test')
INSERT INTO T2 VALUES ('C', 'Test')
INSERT INTO T2 VALUES ('D', 'Test')
INSERT INTO T2 VALUES ('E', 'Test')
INSERT INTO T2 VALUES ('C', 'Control')
INSERT INTO T2 VALUES ('F', 'Control')
GO
SELECT * FROM T1
SELECT * FROM T2
--
-- T1 ⋈ϕ T2  Where  ϕ: T1.C1 = T2.C1
SELECT * FROM T1 JOIN T2 ON T1.C1 = T2.C1
-- σϕ(T1 X T2) Where  ϕ: T1.C1 = T2.C1
SELECT * FROM T1, T2 WHERE T1.C1 = T2.C1

-----------------------------------------------------------------------
-- 5
-----------------------------------------------------------------------
USE
MatrixAlgebra
GO
-- Items in squiggly brackets {} are subscripted
-- π{S.C1, R.C2}(σϕ1(R) ⋈ϕ2 S)
-- where
--		ϕ1 = (R.C2 = ‘A’)
--		ϕ2 = (R.C1 = S.C2)
-- 5 Write out equivalent SQL
/*
Algebraic statement: π{S.C1, R.C2}(σ{R.C2 = ‘A’}(R) ⋈{R.C1 = S.C2} S)

Based on the Homework assignmenet the following is
implied:

we need to create a relation R with at least two columns
and whose second column contains text

we need to create a relation S with at least two columns
and whose second column is of the same type as R's first
column
*/

BEGIN TRY 
DROP TABLE R
END TRY BEGIN CATCH END CATCH
GO
CREATE TABLE R (C1 int, C2 nchar(1), C3 nchar(50)); 
GO
TRUNCATE TABLE R
GO
INSERT INTO R VALUES (1, 'B', 'RBC')
INSERT INTO R VALUES (2, 'A', 'Leukocytes')
INSERT INTO R VALUES (3, 'B', 'Macrophages')
INSERT INTO R VALUES (4, 'A', 'Monocyes')
SELECT * FROM R

BEGIN TRY 
DROP TABLE S
END TRY BEGIN CATCH END CATCH
GO
CREATE TABLE S (C1 nchar(10), C2 int, C3 nchar(50)); 
GO

TRUNCATE TABLE S
GO
INSERT INTO S VALUES ('A', 2, 'Smear')
INSERT INTO S VALUES ('B', 4, 'Cell Sorter')
INSERT INTO S VALUES ('C', 6, 'Elisa')
SELECT * FROM S

-- Step-by-step
BEGIN TRY 
DROP VIEW T
END TRY BEGIN CATCH END CATCH
GO

DROP Table T

-- σ{R.C2 = ‘A’}(R) -> T
CREATE VIEW T as (SELECT * FROM R WHERE C2 = 'A')
GO
SELECT * FROM T
GO
BEGIN TRY 
DROP VIEW U
END TRY BEGIN CATCH END CATCH
GO
-- T ⋈{R.C1 = S.C2} S -> U
CREATE VIEW U as SELECT S.C1 as SC1, S.C2 as SC2, S.C3 as SC3, T.C1 as TC1, T.C2 as TC2, T.C3 as TC3 FROM T JOIN S ON T.C1=S.C2
GO
select * from U
-- π{S.C1, R.C2} U
SELECT SC1, TC2 FROM U

-- typical declarative SQL
SELECT S.C1, R.C2 FROM (SELECT * FROM R WHERE C2 = 'A') AS R JOIN S ON R.C1=S.C2
-- or
SELECT S.C1, R.C2 FROM R JOIN S ON R.C2='A' AND R.C1 = S.C2
-----------------------------------------------------------------------
