SELECT issue.project_id, weekdata.sprint_id, ${velocity} AS avg_velocity
FROM gros.project, gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT sprint_id, project_id, end_date, third_sprint_start, EXTRACT(day FROM (end_date - CASE WHEN third_sprint_end > start_date THEN third_sprint_end ELSE start_date END + third_sprint_end - CASE WHEN mid_sprint_end > third_sprint_start THEN mid_sprint_end ELSE third_sprint_start END + mid_sprint_end - mid_sprint_start)) AS sprint_days
	FROM (SELECT sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date AS third_sprint_start, prevsprints.end_date AS third_sprint_end, MIN(sprint3.start_date) AS mid_sprint_start, MAX(sprint3.end_date) AS mid_sprint_end, COUNT(*)
		FROM gros.sprint, gros.sprint AS prevsprints, gros.sprint AS sprint3
		WHERE prevsprints.project_id = sprint.project_id
		AND sprint3.project_id = sprint.project_id
		AND prevsprints.start_date < sprint.start_date
		AND sprint3.start_date BETWEEN prevsprints.start_date AND sprint.start_date
		GROUP BY sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, prevsprints.start_date, prevsprints.end_date
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
AND ${issue_done}
--AND issue.updated < sprint.end_date
GROUP BY issue.project_id, weekdata.sprint_id, sprint_days
ORDER BY issue.project_id, weekdata.sprint_id;
