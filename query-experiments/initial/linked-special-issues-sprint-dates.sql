SELECT sprint1.start_date, sprint2.start_date FROM (
	SELECT issue1.sprint_id AS sprint1, issue2.sprint_id AS sprint2 FROM (
		SELECT DISTINCT key, type, sprint_id FROM gros.issue
		WHERE sprint_id <> 0
	) AS issue1
	JOIN gros.issuelink ON issuelink.from_key = issue1.key
	JOIN (SELECT DISTINCT key, type, sprint_id FROM gros.issue) AS issue2
	ON issuelink.to_key = issue2.key
	WHERE issue1.type IN (1,3,11,12) AND issue1.sprint_id <> issue2.sprint_id
) AS issues
JOIN gros.sprint AS sprint1 ON issues.sprint1 = sprint1.sprint_id
JOIN gros.sprint AS sprint2 ON issues.sprint2 = sprint2.sprint_id;