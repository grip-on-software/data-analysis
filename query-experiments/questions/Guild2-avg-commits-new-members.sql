SELECT project.name, sprint.start_date, new_committers, avg_new_commits, AVG(curdata.num_commits) AS avg_commits
FROM gros.project, gros.sprint,
(SELECT commits.project_id, commits.sprint_id, commits.developer_id, COUNT(*) AS num_commits FROM gros.commits
GROUP BY commits.project_id, commits.sprint_id, commits.developer_id) AS curdata
LEFT OUTER JOIN
(SELECT commit1.project_id, commit1.sprint_id, COUNT(*) AS new_committers, AVG(commit1.num_commits) AS avg_new_commits
FROM (
    ((gros.sprint AS sprint1 INNER JOIN gros.sprint AS sprint2
    ON sprint1.project_id = sprint2.project_id AND sprint2.end_date < sprint1.start_date AND EXTRACT(day FROM sprint1.start_date - sprint2.end_date) < 7)
    LEFT OUTER JOIN
    (SELECT commits.project_id, commits.sprint_id, commits.developer_id, COUNT(*) AS num_commits FROM gros.commits
    GROUP BY commits.project_id, commits.sprint_id, commits.developer_id) AS commit1
    ON commit1.sprint_id = sprint1.sprint_id)
    LEFT OUTER JOIN
    (SELECT commits.project_id, commits.sprint_id, commits.developer_id, COUNT(*) AS num_commits FROM gros.commits
    GROUP BY commits.project_id, commits.sprint_id, commits.developer_id) AS commit2
    ON commit1.project_id = commit2.project_id AND commit1.developer_id = commit2.developer_id AND commit2.sprint_id = sprint2.sprint_id
)
WHERE commit1.num_commits IS NOT NULL AND commit2.num_commits IS NULL
GROUP BY commit1.project_id, commit1.sprint_id) AS newdata
ON curdata.project_id = newdata.project_id AND curdata.sprint_id = newdata.sprint_id
WHERE project.project_id = curdata.project_id AND sprint.sprint_id = curdata.sprint_id
GROUP BY project.name, sprint.start_date, new_committers, avg_new_commits
ORDER BY project.name, sprint.start_date;

