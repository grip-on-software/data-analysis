SELECT repo.project_id, COUNT(*) AS num_push_events FROM gros.vcs_event
LEFT JOIN gros.repo ON vcs_event.repo_id = repo.id
GROUP BY repo.project_id HAVING repo.project_id IS NOT NULL;
