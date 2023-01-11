CREATE TABLE IF NOT EXISTS furniture (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  price INTEGER NOT NULL,
  description TEXT
);

INSERT INTO furniture (name, price, description) VALUES
('Chair', 10, 'Description of chair'),
('Sofa', 20, 'Description of sofa'),
('Dresser', 30, 'Description of dresser');

