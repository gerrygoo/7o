/*
	Gerardo Galván Olvera A01371872
*/
// --1
// 1.d
delimiter ༼つ◕_◕༽つ
drop procedure if exists add_to_inventory ༼つ◕_◕༽つ
create procedure add_to_inventory(in film_id int, in store_id int)
begin
	declare broke int default 0;
	declare lid int;
    
    declare continue handler for 1452 set broke = 1;
    declare continue handler for 1136 set broke = 1;
    
    select max(inventory_id) from sakila.inventory into lid;
    insert into sakila.inventory values (lid + 1 , film_id, store_id, now() );
    
    if broke = 1 then
		select "Tried to insert non existant key";
	else
		select * from sakila.inventory where inventory_id = lid + 1;
    end if;
    
end༼つ◕_◕༽つ
delimiter ;
call add_to_inventory(20, 2);
call add_to_inventory(20, 20);
// 1.e
delimiter (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧✧ﾟ･:*ヽ(◕ヮ◕ヽ)
drop procedure if exists get_email_list (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧✧ﾟ･:*ヽ(◕ヮ◕ヽ)
create procedure get_email_list(in store_id int)
begin
	declare txt text default "";
    declare curmail varchar(50);
    declare done int default 0;
    
    declare mails cursor for select email from sakila.customer c where c.store_id = store_id;
    declare continue handler for not found set done = 1;
    open mails;
		repeat
			fetch mails into curmail;
			set txt = concat(ifnull(txt, ""), curmail, ";");
        until done end repeat;
	close mails;
    select txt;
end(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧✧ﾟ･:*ヽ(◕ヮ◕ヽ)
delimiter ;

call get_email_list(1);
call get_email_list(2);

// -- 2
// 2.c
delimiter ◉_◉
drop function if exists sakila.number_of_films ◉_◉
create function sakila.number_of_films(actor_id int,  category_id int) returns int deterministic
begin
	declare total int default 0;
    
	select count(*) from film_actor fa, film_category ca
    where fa.actor_id = actor_id
    and ca.category_id = category_id
    and fa.film_id = ca.film_id into total;
    
    return total;

end ◉_◉
delimiter ;
select  number_of_films(2, 1);
select  number_of_films(2, 1);

// --- 3
// 3.b
delimiter ｡◕‿◕｡
drop trigger if exists film_rate_update_check｡◕‿◕｡
create trigger film_rate_update_check before update on film for each row
begin
	declare rented_times int;
    
    select count(*) from rental r, inventory i
    where i.film_id = old.film_id
    and r.inventory_id = i.inventory_id into rented_times;
    
    if  rented_times < 25 then
		signal sqlstate '80000';
    end if;
end｡◕‿◕｡
delimiter ;

    
update film set rental_rate = 2 where film_id = 200;

