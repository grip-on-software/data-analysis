SELECT issue.project_id, weekdata.sprint_id, CAST(SUM(story_points) AS float)/(days/7*5) AS avg_velocity
FROM gros.project, gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT sprint_id, project_id, end_date, third_sprint_start, EXTRACT(day FROM (end_date - third_sprint_start)) AS days
	FROM (SELECT sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date AS third_sprint_start, COUNT(*)
		FROM gros.sprint, gros.sprint AS prevsprints, gros.sprint AS sprint3
		WHERE prevsprints.project_id = sprint.project_id
		AND sprint3.project_id = sprint.project_id
		AND prevsprints.start_date < sprint.start_date
		AND sprint3.start_date BETWEEN prevsprints.start_date AND sprint.start_date
		GROUP BY sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date
		HAVING COUNT(*) = 3
	) AS backdata
) AS weekdata
-- Additional joins
WHERE issue.project_id = project.project_id
AND issue.project_id = weekdata.project_id
AND issue.issue_id = maxdata.issue_id AND issue.changelog_id = max_changelog_id
--AND issue.sprint_id = weekdata.sprint_id
AND issue.updated BETWEEN weekdata.third_sprint_start AND weekdata.end_date
-- Resolved before the end of the sprint (Fixed or Closed)
AND (issue.resolution = 1 or issue.status = 6)
--AND issue.updated < sprint.end_date
GROUP BY issue.project_id, weekdata.sprint_id, days
ORDER BY issue.project_id, weekdata.sprint_id;
