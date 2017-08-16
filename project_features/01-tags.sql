SELECT project_id, COUNT(DISTINCT tag_name) AS num_tags
FROM gros.tag JOIN gros.repo ON tag.repo_id = repo.id GROUP BY project_id;
