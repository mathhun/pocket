-- TODO: more space efficient schema
create table article (
    created_at datetime not null comment "YYYY-mm-dd HH:00",
    item_id bigint not null,
    resolved_id bigint not null,
    given_url varchar(1024) not null,
    given_title varchar(1024) not null,
    favorite boolean not null,
    status int not null,
    time_added datetime not null,
    time_updated datetime not null,
    time_read datetime not null,
    time_favorited datetime not null,
    sort_id int not null,
    resolved_title varchar(1024) not null,
    resolved_url varchar(1024) not null,
    excerpt varchar(1024) not null,
    is_article boolean not null,
    is_index boolean not null,
    has_video boolean not null,
    has_image boolean not null,
    word_count int not null,
    primary key (created_at, item_id)
) engine=innodb default charset=utf8;
