-- !preview conn=spark_conn

SELECT Name, Age, Value, Overall
FROM `data`
WHERE Value = '€0' 


