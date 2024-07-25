SELECT num_story_points, num_commits FROM
(SELECT project_id, sprint_id, SUM(story_points) AS num_story_points FROM
    (SELECT DISTINCT project_id, issue_id, story_points, sprint_id FROM gros.issue) AS latest_issues
    WHERE sprint_id <> 0
    GROUP BY project_id, sprint_id
) AS issuedata,
(SELECT project_id, sprint_id, COUNT(*) AS num_commits FROM gros.commits WHERE sprint_id <> 0 GROUP BY project_id, sprint_id) AS commitdata
WHERE issuedata.project_id = commitdata.project_id AND issuedata.sprint_id = commitdata.sprint_id;
