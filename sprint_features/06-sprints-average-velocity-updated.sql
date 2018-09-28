SELECT issue.project_id, weekdata.sprint_id, CAST(SUM(issue.story_points) AS float)/(CASE WHEN sprint_days <> 0 THEN CEIL(sprint_days/7.0*5) ELSE 1 END) AS avg_velocity
FROM gros.project, gros.issue,
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.issue GROUP BY issue_id) AS maxdata,
(SELECT sprint_id, project_id, end_date, third_sprint_start, EXTRACT(day FROM (end_date - CASE WHEN third_sprint_end > start_date THEN third_sprint_end ELSE start_date END + third_sprint_end - third_sprint_start + mid_sprint_end - CASE WHEN third_sprint_end > mid_sprint_start THEN third_sprint_end ELSE mid_sprint_start END)) AS sprint_days
	FROM (SELECT sprint.sprint_id, sprint.project_id, sprint.name, 
                sprint.start_date,
                CASE WHEN sprint.complete_date IS NOT NULL AND sprint.complete_date < sprint.end_date THEN sprint.complete_date ELSE sprint.end_date END AS end_date,
                prevsprints.start_date AS third_sprint_start,
                CASE WHEN prevsprints.complete_date IS NOT NULL AND prevsprints.complete_date < prevsprints.end_date THEN prevsprints.complete_date ELSE prevsprints.end_date END AS third_sprint_end,
                MIN(sprint3.start_date) AS mid_sprint_start,
                CASE WHEN MAX(sprint3.complete_date) IS NOT NULL AND MAX(sprint3.complete_date) < MAX(sprint3.end_date) THEN MAX(sprint3.complete_date) ELSE MAX(sprint3.end_date) END AS mid_sprint_end,
                COUNT(*)
		FROM gros.sprint, gros.sprint AS prevsprints, gros.sprint AS sprint3
		WHERE prevsprints.project_id = sprint.project_id
		AND sprint3.project_id = sprint.project_id
		AND prevsprints.start_date < sprint.start_date
		AND sprint3.start_date BETWEEN prevsprints.start_date AND sprint.start_date
		GROUP BY sprint.sprint_id, sprint.project_id, sprint.name, sprint.start_date, sprint.end_date, sprint.complete_date, prevsprints.start_date, prevsprints.end_date, prevsprints.complete_date
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
AND ${s(issue_done)}
--AND issue.updated < sprint.end_date
GROUP BY issue.project_id, weekdata.sprint_id, sprint_days
ORDER BY issue.project_id, weekdata.sprint_id;
