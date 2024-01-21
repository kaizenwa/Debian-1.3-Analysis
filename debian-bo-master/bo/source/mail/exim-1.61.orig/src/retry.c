/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */

/* Functions concerned with retrying unsuccessful deliveries. */


#include "exim.h"



/*************************************************
*     Set status of a host+address item          *
*************************************************/

/* This function is passed a host_item which contains a host name and an
address string. Its job is to set the status of the address if it is not
already set (indicated by hstatus_unknown). The possible values are:

   hstatus_usable    the address is not listed in the unusable tree, and does
                     not have a retry record, or the time is past the next
                     try time.

   hstatus_unusable  the address is listed in the unusable tree, or does have
                     a retry record, and the time is not yet at the next retry
                     time.

   hstatus_unusable_expired  as above, but also the retry time has expired
                     for this address.

If a retry record is retrieved from the hints database, the time of last
trying is filled into the last_try field of the host block.

Argument:   pointer to a host item
Returns:    NULL if no retry record was read; otherwise
            pointer to the key name of the retry record
*/

char *
retry_check_address(host_item *host)
{
char *key;
EXIM_DB *dbm_file;
tree_node *node;
db_retry *retry_record;

/* Do nothing if status already set; otherwise initialize status as usable. */

if (host->status != hstatus_unknown) return NULL;
host->status = hstatus_usable;

/* Generate the key for the unusable tree and the retry database. */

key = string_sprintf("T:%s:%s", host->name, host->address);

/* Search the tree of unusable addresses. This is filled in when deliveries
fail, because the retry database itself is not updated until the end of all
deliveries (so as to do it all in one go). The tree records addresses that have
become unusable during this delivery process (i.e. those that will get put into
the retry database when it is updated). */

node = tree_search(tree_unusable, key);
if (node != NULL)
  {
  host->status = hstatus_unusable;
  host->why = node->data.val;
  return NULL;
  }

/* Open the retry database, giving up if there isn't one. Otherwise, search for
the retry record, and then close the database again. */

if ((dbm_file = db_open("retry", O_RDONLY)) == NULL) return NULL;
retry_record = db_read(dbm_file, key);
db_close(dbm_file);

/* If there's no retry record, free the key and return. */

if (retry_record == NULL)
  {
  store_free(key);
  return NULL;
  }

/* Set the status according to the retry data, and return the key. */

if (time(NULL) < retry_record->next_try && !deliver_force)
  {
  host->status = (retry_record->expired)?
    hstatus_unusable_expired : hstatus_unusable;
  host->why = hwhy_retry;
  host->last_try = retry_record->last_try;
  }

return key;
}




/*************************************************
*           Add a retry item to an address       *
*************************************************/

/* Retry items are chained onto an address when it is deferred either by
a director, router, or transport, or if it succeeds or fails and there was a
previous retry item that now needs to be deleted.

They are used at the end to update the retry database. The key name is built
from a type letter and the domain, optionally with "<local_part>@" on the front
- this is used for local destinations - or passed in externally for remote
destinations, which might be more complex (SMTP destinations consist of the
domain + the IP address).

Arguments:
  addr    the address block
  type    the type letter: one of 'D', 'R', or 'T'
  use_at  if TRUE, "addr->local_part@" is included after "type:"
  key     if NULL, addr->domain is used, otherwise key
  delete  the delete flag, copied into the block

Returns:  nothing
*/

void
retry_add_item(address_item *addr, char *type, BOOL use_at, char *key,
  BOOL delete)
{
retry_item *rti = store_malloc(sizeof(retry_item));
rti->next = addr->retries;
addr->retries = rti;
rti->destination = string_sprintf("%s:%s%s%s", type,
  use_at? addr->local_part : "",
  use_at? "@" : "",
  (key != NULL)? key : addr->domain);
rti->basic_errno = addr->basic_errno;
rti->more_errno = addr->more_errno;
rti->message = addr->message;
rti->delete = delete;

DEBUG(7) debug_printf("added retry item for %s: errno=%d %d\n",
  rti->destination, rti->basic_errno, rti->more_errno);
}



/*************************************************
*        Find retry configuration data           *
*************************************************/

/* Search the in-store retry information for the first retry item that applies
to a given destination. Remote Internet destinations are given as
domain:address. It doesn't seem necessary to go to that level of detail in
retry rules, so we chop off the address and match on the domain part only.
Local addresses are given as user@domain. The alternative destination is the
domain name, which is always just a domain. This function is used for real
only in this module, but it is global so it can be tested by means of the -brt
option.

Arguments:
  destination  destination for which retry info is wanted
  alternate    alternative destination, always just a domain
  errno        specific error predicate on the retry rule, or zero
  more_errno   additional data for errno predicate (for errno_quota only)

Returns:       pointer to retry rule, or NULL
*/

retry_config *
retry_find_config(char *destination, char *alternate, int errno, int more_errno)
{
int domain = 0;
int replace = ':';
char *colon = strchr(destination, ':');
retry_config *yield;

/* If there's a colon in the destination, temporarily replace it with
a zero to terminate the string there. If there's no colon there should be
an @; set up the domain offset. */

if (colon == NULL)
  {
  char *domain_offset = strrchr(destination, '@');
  if (domain_offset != NULL) domain = domain_offset - destination + 1;
  colon = destination + (int)strlen(destination);
  replace = 0;
  }
*colon = 0;

/* Scan the configured retry items. */

for (yield = retries; yield != NULL; yield = yield->next)
  {
  char *dlist = yield->destination;

  /* If a specific error is set for this item, check that we are
  handling that specific error, and if so, check any additional
  error information if required. Special code is required for quota
  errors, as these can either be system quota errors, or Exim's own
  quota imposition, which has a different error number. */

  if (yield->basic_errno != 0)
    {
    if (yield->basic_errno == ERRNO_EXIMQUOTA)
      {
      if ((errno != ERRNO_EXIMQUOTA && errno != errno_quota) ||
          (yield->more_errno != 0 && yield->more_errno > more_errno))
        continue;
      }
    else
      {
      if (yield->basic_errno != errno ||
         (yield->more_errno != 0 && yield->more_errno != more_errno))
       continue;
      }
    }

  /* If the destination contains an @ we are handling a local delivery;
  otherwise it's a remote host name. Either way, we can persuade the
  match_address function to handle it. */

  if (match_address_list(destination, domain, dlist, &(yield->re), -1, 0))
    break;

  /* If the match failed, have a go at matching on the alternative destination,
  which is normally the domain as opposed to the host for a remote delivery. */

  if (alternate != NULL &&
      match_address_list(alternate, 0, dlist, &(yield->re), -1, 0)) break;
  }

*colon = replace;
return yield;
}




/*************************************************
*              Update retry database             *
*************************************************/

/* Update the retry data for any destinations that were deferred, or delete it
for destinations that succeeded or failed hard. This is done all in one go to
minimize opening/closing/locking of the database file.

Note that, because SMTP delivery involves a list of destinations to try, there
may be defer-type retry information for some of them even when the message was
successfully delivered. Likewise if it eventually failed.

This function may move addresses from the defer to the failed queue if the
ultimate retry time has expired.

Arguments:
  addr_defer    queue of deferred addresses
  addr_failed   queue of failed addresses
  addr_succeed  queue of successful addresses

Returns:        nothing
*/

void
retry_update(address_item **addr_defer, address_item **addr_failed,
  address_item **addr_succeed)
{
EXIM_DB *dbm_file = NULL;
time_t now = time(NULL);
int i;

DEBUG(2) debug_printf("Processing retry items\n");

/* Three-times loop to handle succeeded, failed, and deferred addresses.
Deferred addresses must be handled after failed ones, because some may be moved
to the failed chain if all their destinations have timed out. */

for (i = 0; i < 3; i++)
  {
  address_item *endaddr, *addr;
  address_item **paddr = (i==0)? addr_succeed :
    (i==1)? addr_failed : addr_defer;
  BOOL defer = (i == 2);

  DEBUG(5) debug_printf("%s addresses:\n", (i == 0)? "Succeeded" : (i == 1)?
    "Failed" : "Deferred");

  /* Loop for each address on the chain. For deferred addresses, the whole
  address times out unless one of its retry addresses has a retry rule that
  hasn't yet timed out. Deferred addresses should not be requesting deletion
  of retry items, but just in case they do by accident, treat that case
  as "not timed out".

  As well as handling the addresses themselves, we must also process any
  retry items for any parent addresses - these are typically "delete" items,
  because the parent must have succeeded in order to generate the child. */

  while ((endaddr = *paddr) != NULL)
    {
    BOOL timed_out = TRUE;
    retry_item *rti;

    for (addr = endaddr; addr != NULL; addr = addr->parent)
      {
      DEBUG(5) debug_printf("%s%s\n", addr->orig, (addr->retries == NULL)?
        ": no retry items" : "");

      /* Loop for each retry item. */

      for (rti = addr->retries; rti != NULL; rti = rti->next)
        {
        char *message;
        int message_length, message_space, failing_interval, next_try;
        retry_rule *rule, *final_rule;
        retry_config *retry;
        db_retry *retry_record;

        /* Open the retry database if it is not already open; failure to open
        the file is logged, but otherwise ignored - deferred addresses will
        get retried at the next opportunity. Not opening earlier than this saves
        opening if no addresses have retry items - common when none have yet
        reached their retry next try time. */

        if (dbm_file == NULL) dbm_file = db_open("retry", O_RDWR|O_CREAT);
        if (dbm_file == NULL)
          {
          log_write(0, LOG_MAIN, "Failed to open retry database");
          return;
          }

        /* Handle the case of a request to delete the retry info for this
        destination. Keep the timed_out flag for the end address only. */

        if (rti->delete)
          {
          (void)db_delete(dbm_file, rti->destination);
          DEBUG(5) debug_printf("deleted retry information for %s\n",
            rti->destination);
          if (addr == endaddr) timed_out = FALSE;
          continue;
          }

        /* Get the retry information for this destination and error code, if
        any. If this item is for a remote host with ip address, then pass
        the domain name as an alternative to search for. If no retry
        information is found, we can't generate a retry time, so there is
        no point updating the database. */

        if ((retry = retry_find_config(rti->destination+2,
             (strchr(rti->destination+2, ':') != NULL)? addr->domain : NULL,
             rti->basic_errno, rti->more_errno)) == NULL)
          {
          DEBUG(5) debug_printf("No configured retry item for %s%s%s\n",
            rti->destination,
            (strchr(rti->destination+2, ':') != NULL)? " or " : "",
            (strchr(rti->destination+2, ':') != NULL)? addr->domain : "");
          continue;
          }

        DEBUG(5)
          {
          if (strchr(rti->destination+2, ':') != NULL)
            debug_printf("retry for %s (%s) = %s\n", rti->destination,
              addr->domain, retry->destination);
          else
            debug_printf("retry for %s = %s\n", rti->destination,
              retry->destination);
          }

        /* Set up the message for the database retry record. */

        message = (rti->basic_errno > 0)? strerror(rti->basic_errno) :
          (rti->message == NULL)? "unknown error" : rti->message;
        message_length = strlen(message);

        /* Read a retry record from the database or construct a new one */

        retry_record = db_read(dbm_file, rti->destination);
        if (retry_record == NULL)
          {
          retry_record = store_malloc(sizeof(db_retry) + message_length);
          message_space = message_length;
          retry_record->first_failed = now;
          retry_record->last_try = now;
          retry_record->next_try = now;
          retry_record->expired = FALSE;
          retry_record->text[0] = 0;      /* just in case */
          }
        else message_space = (int)strlen(retry_record->text);

        /* Compute how long this destination has been failing */

        failing_interval = now - retry_record->first_failed;

        /* Search for the current retry rule. The cutoff time of the
        last rule is handled differently to the others. The rule continues
        to operate for ever (the global maximum interval will eventually
        limit the gaps) but its cutoff time determines when an individual
        destination times out. If there are no retry rules, the destination
        always times out, but we can't compute a retry time. */

        final_rule = NULL;
        for (rule = retry->rules; rule != NULL; rule = rule->next)
          {
          if (failing_interval <= rule->timeout) break;
          final_rule = rule;
          }

        /* If there's an un-timed out rule, the destination has not
        yet timed out, so the address as a whole has not timed out (but we are
        interested in this only for the end address). Make sure the expired
        flag is false (can be forced via fixdb from outside, but ensure it is
        consistent with the rules whenever we go through here). */

        if (rule != NULL)
          {
          if (addr == endaddr) timed_out = FALSE;
          retry_record->expired = FALSE;
          }

        /* Otherwise, set the retry timeout expired, and set the final rule
        as the one from which to compute the next retry time. Subsequent
        messages will fail immediately until the retry time is reached. */

        else
          {
          rule = final_rule;
          retry_record->expired = TRUE;
          }

        /* There is a special case to consider when some messages get through
        to a destination and others don't. This can happen locally when a
        large message pushes a user over quota, and it can happen remotely
        when a machine is on a dodgy Internet connection. The messages that
        get through wipe the retry information, causing those that don't to
        stay on the queue longer than the final retry time. In order to
        avoid this, we check, using the time of arrival of the message, to
        see if it has been on the queue for more than the final cutoff time,
        and if so, cause the message to get bounced, and the retry time to
        be set to "now" so that any subsequent messages in the same condition
        also get tried. We search for the last rule onwards from the one that
        is in use. If there are no retry rules, it will be null and timed_out
        will already be set. */

        if (received_time < retry_record->first_failed &&
            addr == endaddr && !timed_out && rule != NULL)
          {
          retry_rule *last_rule;
          for (last_rule = rule;
               last_rule->next != NULL;
               last_rule = last_rule->next);
          if (now - received_time > last_rule->timeout)
            {
            timed_out = TRUE;
            rule = NULL;
            }
          }

        /* Compute the next try time from the rule, subject to the global
        maximum, and update the retry database. If rule == NULL it means
        there were no rules at all (and the timeout will be set expired),
        or we have a message that is older than the final timeout. In this
        case set the next retry time to now, so that one delivery attempt
        happens for subsequent messages. */

        if (rule == NULL) next_try = now; else
          {
          if (rule->rule == 'F') next_try = now + rule->p1;
          else  /* assume rule = 'G' */
            {
            int last_predicted_gap =
              retry_record->next_try - retry_record->last_try;
            int last_actual_gap = now - retry_record->last_try;
            int lastgap = (last_predicted_gap < last_actual_gap)?
              last_predicted_gap : last_actual_gap;
            next_try = now + ((lastgap < rule->p1)? rule->p1 :
               (lastgap * rule->p2)/1000);
            }
          }

        /* Impose a global retry max */

        if (next_try - now > retry_interval_max)
          next_try = now + retry_interval_max;

        /* If the new message length is greater than the previous one, we
        have to copy the record first. */

        if (message_length > message_space)
          {
          db_retry *newr = store_malloc(sizeof(db_retry) + message_length);
          memcpy(newr, retry_record, sizeof(db_retry));
          retry_record = newr;
          }

        /* Set up the retry record */

        retry_record->last_try = now;
        retry_record->next_try = next_try;
        retry_record->basic_errno = rti->basic_errno;
        retry_record->more_errno = rti->more_errno;
        strcpy(retry_record->text, message);

        DEBUG(5)
          {
          debug_printf("Writing retry data for %s\n", rti->destination);
          debug_printf("  first failed=%d last try=%d next try=%d expired=%d\n",
            retry_record->first_failed, retry_record->last_try,
            retry_record->next_try, retry_record->expired);
          debug_printf("  error %d %d: %s\n", retry_record->basic_errno,
            retry_record->more_errno, retry_record->text);
          }

        db_write(dbm_file, rti->destination, retry_record,
          sizeof(db_retry) + message_length);
        }                            /* Loop for each retry item */
      }                              /* Loop for address & parents */

    /* If this is a deferred address, and retry processing was requested by
    means of one or more retry items, and no actions were skipped because a
    retry time had not been reached, and no retry item is still active,
    move the address to the failed queue, and restart this loop without
    updating paddr. */

    if (endaddr->retries != NULL && !endaddr->retry_skipped &&
        timed_out && defer)
      {
      *paddr = endaddr->next;
      endaddr->retry_timedout = TRUE;
      endaddr->next = *addr_failed;
      *addr_failed = endaddr;
      endaddr->message = (endaddr->message == NULL)? "retry timeout exceeded" :
        string_sprintf("%s: retry timeout exceeded", endaddr->message);
      log_write(0, LOG_MAIN, "** %s: retry timeout exceeded", endaddr->orig);
      }

    else paddr = &(endaddr->next);    /* Advance to next address */
    }                                 /* Loop for all addresses  */
  }                                   /* Loop for succeed, fail, defer */

/* Close and unlock the database */

if (dbm_file != NULL) db_close(dbm_file);

DEBUG(2) debug_printf("end of retry processing\n");
}

/* End of retry.c */
