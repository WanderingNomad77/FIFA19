-- !preview conn=sc

SELECT Name, Age, Overall FROM `data` 
WHERE Overall >= 90 and Age <= 30
LIMIT 1000
