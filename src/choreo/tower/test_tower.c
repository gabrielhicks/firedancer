#include "fd_tower.h"

#define INSERT( c, p )                                                                             \
  slots[i]        = c;                                                                             \
  parent_slots[i] = p;                                                                             \
  fd_ghost_insert( ghost, slots[i], parent_slots[i] );                                             \
  i++;

/**

      1
      2
     / \
    3   5
   4
 */

void 
test_tower_costs( fd_wksp_t * wksp ){
  void * mem = fd_wksp_alloc_laddr( wksp, fd_tower_align(), fd_tower_footprint(), 1UL );
  fd_tower_t * tower = fd_tower_join( fd_tower_new( mem ) );
  ulong node_max = 32;
  ulong vote_max = 32;
  /**
    Initializing forks...
   */

  fd_exec_slot_ctx_t slot_ctx = { 0 };
  slot_ctx.slot_bank.slot = 0;

  fd_forks_t * forks = fd_forks_join( fd_forks_new( fd_wksp_alloc_laddr( wksp, 
                                                                               fd_forks_align(), 
                                                                                  fd_forks_footprint( 32 ), 
                                                                                 1UL ), 
                                                             32, 
                                                            1UL ) );
  FD_TEST( fd_forks_init( forks, &slot_ctx ) );


  /**
    Init ghost
   */
  fd_ghost_t * ghost = fd_ghost_join( fd_ghost_new( fd_wksp_alloc_laddr( wksp, fd_ghost_align(), fd_ghost_footprint( node_max, vote_max ), 1UL ), node_max, vote_max, 1UL ) );
  fd_ghost_init( ghost, 1, 32 );

  ulong slots[32];
  ulong parent_slots[32];
  ulong i = 0;
  INSERT( 2, 1 );
  INSERT( 3, 2 );
  INSERT( 4, 3 );
  INSERT( 5, 2 );

  fd_tower_vote( tower, 1 );
  fd_tower_vote( tower, 2 );
  fd_tower_vote( tower, 3 );
  fd_tower_vote( tower, 4 );

  fd_tower_print( tower );

  /** check the cost of each vote in tower */
  ulong conf = 1;
  for( fd_tower_votes_iter_t iter = fd_tower_votes_iter_init_rev( tower->votes );
       !fd_tower_votes_iter_done_rev( tower->votes, iter );
       iter = fd_tower_votes_iter_prev( tower->votes, iter ) ) {
    fd_tower_vote_t const * vote = fd_tower_votes_iter_ele_const( tower->votes, iter );
    FD_TEST( vote->conf == conf );
    conf++;
  }

  /*fd_fork_t const * vote_fork = fd_tower_vote_fork( tower,
                                                    forks,
                                                    acc_mgr,
                                                    ghost );*/

  fd_tower_print( tower );
}

/*
Switching forks where
     0
   /   \ 
 odds evens
*/
void 
test_lockout_check( fd_wksp_t * wksp ){
  void * mem = fd_wksp_alloc_laddr( wksp, fd_tower_align(), fd_tower_footprint(), 1UL );
  fd_tower_t * tower = fd_tower_join( fd_tower_new( mem ) );

  /**
    Initializing forks...
   */
  fd_exec_slot_ctx_t slot_ctx = { 0 };
  slot_ctx.slot_bank.slot = 0;

  fd_forks_t * forks = fd_forks_join( fd_forks_new( fd_wksp_alloc_laddr( wksp, 
                                                                               fd_forks_align(), 
                                                                                  fd_forks_footprint( 32 ), 
                                                                                 1UL ), 
                                                             32, 
                                                            1UL ) );
  FD_TEST( fd_forks_init( forks, &slot_ctx ) );

  /**
    Initializing ghost...
   */
  fd_ghost_t * ghost = fd_ghost_join( fd_ghost_new( fd_wksp_alloc_laddr( wksp, fd_ghost_align(), fd_ghost_footprint( 32, 32 ), 1UL ), 32, 32, 1UL ) );
  fd_ghost_init( ghost, 0, 32 );

  /** populate trees + votes */

  for( int i = 1; i < 32; i++ ){
    ulong parent_slot = i - 2 >= 0 ? (ulong) i - 2 : 0;
    fd_fork_t * fork = fd_fork_pool_ele_acquire( forks->pool );
    fork->slot = (ulong) i;
    fd_fork_frontier_ele_insert( forks->frontier, fork, forks->pool );
    fd_ghost_insert( ghost, (ulong) i, parent_slot );
    if ( i % 2 == 0 ){
      fd_tower_vote( tower, (ulong) i );
    }
  }
  fd_pubkey_t pk = { .key = { 1 } };
  fd_ghost_replay_vote( ghost, 31, &pk, 1 );

  // Odd fork is now best fork

  FD_TEST( fd_tower_is_max_lockout( tower ) == false );

  //fd_fork_t best_fork = { .slot = 31 };

  fd_fork_t const * fork = fd_tower_best_fork( tower, forks, ghost);
  FD_TEST( fork->slot == 31 );
  FD_TEST( fd_tower_lockout_check( tower, fork, ghost ) == 0 );
  ulong slot = 30;
  fd_fork_t const * my_fork = fd_fork_frontier_ele_query( forks->frontier, &slot, NULL, forks->pool );
  FD_TEST( fd_tower_lockout_check( tower, my_fork, ghost) == 1);

  FD_TEST( fd_tower_switch_check( tower, fork, ghost ) == 1 );

  fd_tower_print( tower );
}

void
test_caveman_lover_boy( fd_wksp_t * wksp ){
  void * mem = fd_wksp_alloc_laddr( wksp, fd_tower_align(), fd_tower_footprint(), 1UL );
  fd_tower_t *tower = fd_tower_join( fd_tower_new( mem ) );
  FD_TEST_CUSTOM(tower, "Failed to join the tower");

  /* Add some votes to the tower
     (0, 31) expiration = 0 + 1<<31 (some big number)
     (1, 30) expiration = 1 + 1<<30 (some big number)
     (2, 29) expiration = 2 + 1<<29 (some big number)
     ..
     (28, 3) expiration = 28 + 1<<3 = 36
     (29, 2) expiration = 29 + 1<<2 = 33
     (30, 1) expiration = 30 + 1<<1 = 32 */
  for (ulong i = 0; i < 31; i++)
  {
    fd_tower_vote( tower, i );
    FD_TEST( fd_tower_votes_cnt( tower->votes ) == i + 1 );
  }
  for (ulong i = 0; i < 31; i++)
  {
    fd_tower_vote_t expected_vote = { .slot = i, .conf = 31-i };
    fd_tower_vote_t *actual_vote = fd_tower_votes_peek_index( tower->votes, i );
    FD_TEST( expected_vote.slot == actual_vote->slot );
    FD_TEST( expected_vote.conf == actual_vote->conf );
  }

  /* CASE 1: NEW VOTE WHICH REPLACES EXPIRED VOTE */

  /* Check expiration
   A vote for 33 should make the vote for 30 expire.
   A full tower has 31 votes. One expired vote => 30 remaining.*/
  ulong new_vote_expiry = 33;
  ulong vote_cnt = fd_tower_simulate_vote( tower, new_vote_expiry );
  FD_TEST( vote_cnt == 30 );

  /* Check slots 1 through 30 are unchanged after voting */
  fd_tower_vote( tower, new_vote_expiry );
  for (ulong i = 0; i < 30; i++)
  {
    fd_tower_vote_t expected_vote = { .slot = i, .conf = 31-i };
    fd_tower_vote_t *actual_vote = fd_tower_votes_peek_index( tower->votes, i );
    FD_TEST( expected_vote.slot == actual_vote->slot );
    FD_TEST( expected_vote.conf == actual_vote->conf );
  }

  /* Check new vote */
  fd_tower_vote_t expected_vote = { .slot = new_vote_expiry, .conf = 1 };
  fd_tower_vote_t *actual_vote = fd_tower_votes_peek_index( tower->votes, 30 );
  FD_TEST( expected_vote.slot == actual_vote->slot );
  FD_TEST( expected_vote.conf == actual_vote->conf );


  /* CASE 2: NEW VOTE WHICH PRODUCES NEW ROOT */

  ulong new_vote_root = 34;
  fd_tower_vote( tower, new_vote_root );
  FD_TEST( fd_tower_is_max_lockout( tower ) );

  /* Check root */
  ulong expected_root = 0;
  ulong actual_root = fd_tower_publish( tower );
  FD_LOG_NOTICE(( "actual root %lu; expected root %lu", actual_root, expected_root ));
  FD_TEST( actual_root == expected_root );

  /* Check all existing votes moved up by one, with one additional confirmation */
  for (ulong i = 0; i < 29 /* one of the original slots was rooted */; i++)
  {
    fd_tower_vote_t expected_vote = { .slot = i+1, .conf = 31-i };
    fd_tower_vote_t *actual_vote = fd_tower_votes_peek_index( tower->votes, i );
    FD_LOG_INFO(( "evs %lu; avs %lu", expected_vote.slot, actual_vote->slot  ));
    FD_TEST( expected_vote.slot == actual_vote->slot );
    FD_TEST( expected_vote.conf == actual_vote->conf );
  }

  /* Check new vote */
  fd_tower_vote_t expected_vote_root = { .slot = new_vote_root, .conf = 1 };
  fd_tower_vote_t *actual_vote_root = fd_tower_votes_peek_index( tower->votes, 30 );
  FD_TEST( expected_vote_root.slot == actual_vote_root->slot );
  FD_TEST( expected_vote_root.conf == actual_vote_root->conf );

  fd_tower_leave( tower );
  fd_tower_delete( tower );
}

int
main( int argc, char ** argv ) {
  fd_boot( &argc, &argv );

  /* Initialize the test workspace */
  char const * _page_sz = fd_env_strip_cmdline_cstr ( &argc, &argv, "--page-sz",  NULL, "gigantic" );
  ulong        page_cnt = fd_env_strip_cmdline_ulong( &argc, &argv, "--page-cnt", NULL, 1UL        );
  ulong        numa_idx = fd_env_strip_cmdline_ulong( &argc, &argv, "--numa-idx", NULL, fd_shmem_numa_idx( 0 ) );

  FD_LOG_NOTICE(( "Creating workspace with --page-cnt %lu --page-sz %s pages on --numa-idx %lu", page_cnt, _page_sz, numa_idx ));

  ulong page_sz = fd_cstr_to_shmem_page_sz( _page_sz );
  if( FD_UNLIKELY( !page_sz ) ) FD_LOG_ERR(( "unsupported --page-sz" ));
  fd_wksp_t * wksp = fd_wksp_new_anonymous( page_sz, page_cnt, fd_shmem_cpu_idx( numa_idx ), "wksp", 0UL );
  FD_TEST( wksp );

  //test_caveman_lover_boy( wksp );
  //test_lockout_check( wksp );
  test_tower_costs( wksp );

  fd_halt();
  return 0;
}
