SELECT DAYOFWEEK(updated) AS wday, COUNT(*) FROM gros.issue  GROUP BY wday ORDER BY wday; -- gros.commit
