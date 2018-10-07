select c.first_name,c.last_name
from customer c,address a,city ci,country co
where c.address_id=a.address_id and a.city_id=ci.city_id and ci.country_id=co.country_id and co.country='Mexico';

select co.country,sum(c.customer_id)
from customer c,address a,city ci,country co
where c.address_id=a.address_id and a.city_id=ci.city_id and ci.country_id=co.country_id
group by co.country
having sum(c.customer_id)>10;

select c.first_name,c.last_name
from film f,film_category fc,inventory i,rental r,customer c
where c.customer_id=r.customer_id and r.inventory_id=i.inventory_id and i.film_id=f.film_id and f.film_id=fc.film_id and fc.category_id=
	(
	select c.category_id
	from category c
	where c.name='Horror'
    )
group by r.customer_id
having count(r.customer_id)>2;

select a.first_name,a.last_name,a.actor_id
from film f,film_category fc,film_actor fa,actor a
where a.actor_id=fa.actor_id and fa.film_id=f.film_id and f.film_id=fc.film_id and fc.category_id=
	(
	select c.category_id
	from category c
	where c.name='Comedy'
    ) and a.actor_id in
    (
	select a.actor_id
	from film f,film_category fc,film_actor fa,actor a
	where a.actor_id=fa.actor_id and fa.film_id=f.film_id and f.film_id=fc.film_id and fc.category_id=
		(
		select c.category_id
		from category c
		where c.name='Action'
		)
	group by a.actor_id
    )
group by a.actor_id;

select a.first_name,a.last_name, count(a.actor_id)
from film f,film_actor fa,actor a
where f.film_id=fa.film_id and fa.actor_id=a.actor_id
group by a.actor_id
order by count(a.actor_id) desc limit 1;
