SELECT project_id, COUNT(*) AS num_requests
FROM gros.merge_request JOIN gros.repo ON merge_request.repo_id = repo.id
GROUP BY project_id
