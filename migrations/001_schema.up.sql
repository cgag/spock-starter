create type COLOR as enum ('RED', 'BLUE', 'ORANGE', 'PURPLE');
create table turtles (
  name text not null,
  color COLOR not null
);
