-- Q1
SELECT COUNT(*) 
FROM restriction as r
WHERE r.description='R' AND r.country='USA';
-- 59 movies were  categorised in the ‘R’ restriction in the USA.

--Q2
SELECT COUNT(DISTINCT p.id)
FROM person AS p 
INNER JOIN writer AS w ON p.id=w.id
WHERE p.year_born>=1960; 
-- 26.Using inner join to get an intersection of 2 table since WRITER[id] is in PERSON[id].


--Q3
SELECT rc.country,COUNT(rc.description)
FROM restriction_category AS rc
GROUP BY rc.country 
ORDER BY COUNT(rc.description) ASC;
/*  country   | count 
-------------+-------
 Belgium     |     1
 France      |     1
 Netherlands |     1
 HongKong    |     2
 Argentina   |     2
 Spain       |     3
 UK          |     3
 Portugal    |     3
 New Zealand |     3
 Sweden      |     3
 Chile       |     3
 Finland     |     4
 USA         |     4
 Australia   |     5
 Germany     |     6
(15 rows)

*/

--Q4
SELECT (SELECT COUNT(DISTINCT id) FROM director)-COUNT(DISTINCT d.id) AS num
FROM director AS d
INNER JOIN movie AS m ON d.title=m.title AND d.production_year=m.production_year
WHERE m.major_genre='action';
--68.Using the idea of compliment.

--Q5
SELECT  ((100*COUNT(*))/(SELECT COUNT(*)::NUMERIC(4,2) FROM movie AS m WHERE m.country='Australia'))::NUMERIC(4,2) AS percentage--numeric to get decimal we want
FROM  movie as m
WHERE m.country='Australia' AND m.major_genre='action';
--33.33,using numeric to get wanted result.

--Q6
SELECT title ,
production_year,
COUNT(*)
FROM crew_award
GROUP BY title,
production_year,
year_of_award
ORDER BY COUNT DESC
LIMIT 1;--just show 1 result which is the maximum
/*
  title  | production_year | count
---------+-----------------+-------
 Titanic |            1997 |     9
(1 row)
*/

--Q7
SELECT COUNT(DISTINCT (title , production_year))
FROM (SELECT title,production_year FROM movie_award UNION SELECT title , production_year FROM crew_award
	UNION SELECT title,production_year FROM director_award UNION SELECT title,production_year FROM
	writer_award UNION SELECT title,production_year FROM actor_award) AS m;
/*
 count
-------
    32
(1 row)
*/

--Q8
SELECT DISTINCT p.id,p.first_name,p.last_name
FROM writer AS w1
LEFT JOIN (SELECT w.title,w.production_year,COUNT(w.id) 
	FROM writer AS w
	GROUP BY w.title,w.production_year
	HAVING COUNT(w.id)=1) AS w2--subquery the movie with only one writer which means the writer have worked independently
ON w1.title=w2.title AND w1.production_year=w2.production_year--we cannot get id from the step before thus need join writer and make a difference of 2 sets to get the oppsite result of writers who have worked independently.
INNER JOIN person AS p
ON p.id=w1.id
WHERE w2.title IS NULL AND w2.production_year IS NULL;
/*
    id    | first_name |  last_name   
----------+------------+--------------
 00000322 | James      | McCausland
 00000182 | Daphne     | Du Maurier
 00000121 | Roberto    | Benigini
 00000341 | Baz        | Luhrmann
 00000381 | Mary       | Harron (I)
 00000202 | Larry      | Wachowski
 00000141 | Kimberly   | Peirce
 00000382 | Bret       | Easton Ellis
 00000342 | Craig      | Pearce
 00000321 | George     | Miller (II)
 00000201 | Andy       | Wachowski
 00000142 | Andy       | Bienen
 00000461 | Robert     | Bloch
 00000183 | Evan       | Hunter
 00000122 | Vincenzo   | Cerami
 00001344 | Joseph     | Stefano
(16 rows)

*/
--The logic is a little bit compilcated here.Since the writer has never writed a script alone.We may first consider the movie have only one writer then the writer satisfy it must has written alone.Thus consider the complement of the set on writer then we get the writer we want.
 
--Q9
SELECT CONCAT('(',title,',',production_year,')') AS MOVIE FROM 
(SELECT
title,production_year,year_of_award  FROM movie_award UNION ALL SELECT title,production_year,year_of_award FROM crew_award UNION ALL SELECT title,production_year,year_of_award FROM writer_award UNION ALL SELECT title,production_year,year_of_award FROM director_award UNION ALL SELECT title,production_year,year_of_award FROM actor_award) as m--union all tables on what we want and count award of the same year
GROUP BY title,production_year,year_of_award
HAVING COUNT(*)>1;
/*
 (Dahong Denglong Gaogao Gua,1991)
 (Shakespeare in Love,1998)
 (Bullets Over Broadway,1994)
 (The Cider House Rules,1999)
 (The Matrix,1999)
 (Bawang Bie Ji,1993)
 (Strictly Ballroom,1992)
 (Titanic,1997)
 (Chaplin,1992)
 (The Piano,1993)
 (Cyrano de Bergerac,1990)
 (Gladiator,2000)
 (American Beauty,1999)
 (Six Degrees of Separation,1993)
 (Traffic,2000)
 (In the Line of Fire,1993)
 (Alien,1979)
 (Aliens,1986)
 (The Piano,1993)
 (Fearless,1993)
 (Alice,1990)
(25 rows)
*/

