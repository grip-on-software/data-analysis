SELECT project.name, sprint.start_date, COUNT(DISTINCT commits.developer_id) FROM gros.commits, gros.project, gros.sprint
WHERE commits.project_id = project.project_id AND commits.sprint_id = sprint.sprint_id
GROUP BY project.name, sprint.start_date
ORDER BY project.name, sprint.start_date;
