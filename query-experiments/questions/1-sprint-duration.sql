SELECT weeks, COUNT(*)
FROM (SELECT EXTRACT(day FROM (end_date - start_date))/7 AS weeks FROM gros.sprint) AS weekdata
GROUP BY weeks ORDER BY weeks ASC;
