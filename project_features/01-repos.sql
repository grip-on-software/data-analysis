SELECT project_id, COUNT(*) AS num_repos FROM gros.repo
GROUP BY project_id HAVING project_id IS NOT NULL;
