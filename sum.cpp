/// @file
/// @author github.com/SirJonthe
/// @date 2023
/// @copyright Public domain.
/// @license CC0 1.0

#include <cstring>
#include "sum.h"

//
// global
//

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

/// @brief Counts the number of characters in a null-terminated string.
/// @param s The null-terminated string.
/// @return The number of characters in the null-terminated string.
static u64 count_ch(const char *s)
{
	u64 c = 0;
	while (s[c++] != 0) {}
	return c - 1;
}

static constexpr u8 ENDIAN_BYTES_32[sizeof(u32)] = { 1, 2, 3, 4 }; // Constant to determine endianness of current machine.

static constexpr u8 ENDIAN_BYTES_64[sizeof(u64)] = { 1, 2, 3, 4, 5, 6, 7, 8 }; // Constant to determine endianness of current machine.

/// @brief Determines if the machine endian is big at run-time.
/// @returns a boolean indicating true if the machine is big endian, and false otherwise.
static bool is_big32( void )
{
	return *reinterpret_cast<const u32*>(ENDIAN_BYTES_32) == 0x01020304;
}

/// @brief Determines if the machine endian is little at run-time.
/// @returns a boolean indicating true if the machine is little endian, and false otherwise.
static bool is_lil32( void )
{
	return *reinterpret_cast<const u32*>(ENDIAN_BYTES_32) == 0x4030201;
}

//
// crc32
//

cc0::sum::crc32::table cc0::sum::crc32::m_table;

cc0::sum::crc32::table::table( void )
{
	for (u32 i = 0; i < 256; ++i) {
		v[i] = i;
		for (u32 j = 0; j < 8; ++j) {
			if (v[i] & 1) {
				v[i] = 0xedb88320 ^ (v[i] >> 1);
			} else {
				v[i] >>= 1;
			}
		}
	}
}

cc0::sum::crc32::crc32( void ) : m_sum(0)
{}

cc0::sum::crc32::crc32(const char *message) : crc32()
{
	ingest(message);
}

cc0::sum::crc32::crc32(const void *message, u64 byte_count) : crc32()
{
	ingest(message, byte_count);
}

cc0::sum::crc32 &cc0::sum::crc32::operator()(const char *message)
{
	ingest(message);
	return *this;
}

cc0::sum::crc32 &cc0::sum::crc32::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

cc0::sum::crc32 cc0::sum::crc32::operator()(const char *message) const
{
	return crc32(*this)(message);
}

cc0::sum::crc32 cc0::sum::crc32::operator()(const void *message, u64 byte_count) const
{
	return crc32(*this)(message, byte_count);
}

void cc0::sum::crc32::ingest(const char *message)
{
	ingest(message, count_ch(message));
}

void cc0::sum::crc32::ingest(const void *message, u64 byte_count)
{
	m_sum ^= 0xffffffff;
	const u8 *m = (const u8*)message;
	for (u64 i = 0; i < byte_count; ++i) {
		m_sum = m_table.v[(m_sum ^ m[i]) & 0xff] ^ (m_sum >> 8);
	}
	m_sum ^= 0xffffffff;
}

cc0::sum::crc32::sum cc0::sum::crc32::digest( void ) const
{
	sum s;
	const u8 *r = (const u8*)(&m_sum);
	if (is_lil32()) {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[sizeof(m_sum) - i - 1];
		}
	} else {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[i];
		}
	}
	return s;
}

cc0::sum::crc32::operator cc0::sum::crc32::sum( void ) const
{
	return digest();
}

//
// fnv1a64
//

cc0::sum::fnv1a64::fnv1a64( void ) : m_sum(0xcbf29ce484222325ULL)
{}

cc0::sum::fnv1a64::fnv1a64(const char *message) : fnv1a64()
{
	ingest(message);
}

cc0::sum::fnv1a64::fnv1a64(const void *message, u64 byte_count) : fnv1a64()
{
	ingest(message, byte_count);
}

cc0::sum::fnv1a64 &cc0::sum::fnv1a64::operator()(const char *message)
{
	ingest(message);
	return *this;
}

cc0::sum::fnv1a64 &cc0::sum::fnv1a64::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

cc0::sum::fnv1a64 cc0::sum::fnv1a64::operator()(const char *message) const
{
	return fnv1a64(*this)(message);
}

cc0::sum::fnv1a64 cc0::sum::fnv1a64::operator()(const void *message, u64 byte_count) const
{
	return fnv1a64(*this)(message, byte_count);
}

void cc0::sum::fnv1a64::ingest(const char *message)
{
	ingest(message, count_ch(message));
}

void cc0::sum::fnv1a64::ingest(const void *message, u64 byte_count)
{
	const char *m = (const char*)message;
	for (u64 i = 0; i < byte_count; ++i) {
		m_sum ^= u64(m[i]);
		m_sum *= 0x100000001b3ULL;
	}
}

cc0::sum::fnv1a64::sum cc0::sum::fnv1a64::digest( void ) const
{
	sum s;
	const u8 *r = (const u8*)(&m_sum);
	if (is_lil32()) {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[sizeof(m_sum) - i - 1];
		}
	} else {
		for (u64 i = 0; i < sizeof(sum); ++i) {
			s[i] = r[i];
		}
	}
	return s;
}

cc0::sum::fnv1a64::operator cc0::sum::fnv1a64::sum( void ) const
{
	return digest();
}

//
// md5
//

static constexpr u32 CHUNK_BYTESIZE = 512 / CHAR_BIT; // The number of bytes in a cc0::sum::md5 chunk.


static constexpr u32 ShiftTable[CHUNK_BYTESIZE] = { // Table of shift offsets
	7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
	5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
	4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
	6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
};

static constexpr u32 SineTable[CHUNK_BYTESIZE] = { // Precomputed table of integer sines (in radians)
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
	0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
	0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
	0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
	0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
	0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
	0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
	0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
	0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

void cc0::sum::md5::blit(const u8 *src, u8 *dst)
{
	memcpy(dst, src, BYTES_PER_CHUNK);
}

void cc0::sum::md5::blit(const u8 *src, u8 *dst, u32 num)
{
	memcpy(dst, src, num);
	memset(dst + num, 0, BYTES_PER_CHUNK - num);
}

bool cc0::sum::md5::is_aligned(const void *mem)
{
	return (reinterpret_cast<uintptr_t>(mem) & (sizeof(u32) - 1)) != 0;
}

u32 cc0::sum::md5::leftrotate(u32 x, u32 c)
{
    return (x << c) | (x >> (32 - c));
}

void cc0::sum::md5::process_chunk(const u32 *M, u32 *X) const
{
	enum {a0,b0,c0,d0};

	u32 A = X[a0];
	u32 B = X[b0];
	u32 C = X[c0];
	u32 D = X[d0];
	
	// Process every 64 bytes in chunk.
	for (u32 i = 0; i < BYTES_PER_CHUNK; ++i) {
		u32 F = 0;
		u32 g = 0;
		if (i < 16) {
			F = (B & C) | ((~B) & D);
			g = i;
		} else if (i < 32) {
			F = (D & B) | ((~D) & C);
			g = (5*i + 1) % 16;
		} else if (i < 48) {
			F = B ^ C ^ D;
			g = (3*i + 5) % 16;
		} else {
			F = C ^ (B | (~D));
			g = (7*i) % 16;
		}
		
		F = F + A + SineTable[i] + M[g];
		A = D;
		D = C;
		C = B;
		B += leftrotate(F, ShiftTable[i]);
	}
	
	// Add to result.
	X[a0] += A;
	X[b0] += B;
	X[c0] += C;
	X[d0] += D;
}

void cc0::sum::md5::process_final_chunks(u32 *X) const
{

	u64 byte_count = m_chunk_size;
	union {
		u32 w32[WORDS_PER_CHUNK];
		u8  w8[BYTES_PER_CHUNK];
	} chunk;

	// Store size (64 bits) of original message in bits at the end of the message
	u32 padding_size = BYTES_PER_CHUNK - (m_message_size % BYTES_PER_CHUNK);
	if (padding_size < sizeof(u64) + sizeof(u8)) { // Padding must at least fit a 64-bit number to denote message length in bits and one 8-bit number as a terminating 1-bit. If it does not, we add another chunk to process. Note that since we always work on bytes, not bits, the length of the terminating 1-bit is 8 bits, with a value of 0x80.
		padding_size += BYTES_PER_CHUNK;
	}

	// The message will always be padded in some way. Add a first '1' to the padding.
	memcpy(chunk.w8, m_chunk.u8, byte_count);
	chunk.w8[byte_count] = 0x80;
	memset(chunk.w8 + byte_count + 1, 0, BYTES_PER_CHUNK - (byte_count + 1));
	byte_count += padding_size;

	if (byte_count > BYTES_PER_CHUNK) { // Two blocks left to process.
		process_chunk(chunk.w32, X);
		memset(chunk.w8, 0, BYTES_PER_CHUNK - sizeof(u64));
	}

	// One block left to process.
	const u64 ORIGINAL_MESSAGE_BITSIZE = (m_message_size * CHAR_BIT);
	if (is_lil32()) {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			chunk.w8[BYTES_PER_CHUNK - sizeof(u64) + i] = reinterpret_cast<const char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	} else {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			chunk.w8[BYTES_PER_CHUNK - 1 - i] = reinterpret_cast<const
			char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	}
	process_chunk(chunk.w32, X);
}

cc0::sum::md5::md5( void ) : m_message_size(0), m_chunk_size(0)
{
	m_state.u32[0] = 0x67452301; // A
	m_state.u32[1] = 0xefcdab89; // B
	m_state.u32[2] = 0x98badcfe; // C
	m_state.u32[3] = 0x10325476; // D
}

cc0::sum::md5::md5(const char *message) : md5()
{
	ingest(message);
}

cc0::sum::md5::md5(const void *message, u64 byte_count) : md5()
{
	ingest(message, byte_count);
}

cc0::sum::md5::~md5( void )
{
	// Clear sensitive data.
	memset(m_state.u8, 0, sizeof(m_state.u8));
}

cc0::sum::md5 &cc0::sum::md5::operator()(const char *message)
{
	ingest(message);
	return *this;
}

cc0::sum::md5 &cc0::sum::md5::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

cc0::sum::md5 cc0::sum::md5::operator()(const char *message) const
{
	return cc0::sum::md5(*this)(message);
}

cc0::sum::md5 cc0::sum::md5::operator()(const void *message, u64 byte_count) const
{
	return cc0::sum::md5(*this)(message, byte_count);
}

void cc0::sum::md5::ingest(const char *message)
{
	ingest(message, u64(strlen(message)));
}

void cc0::sum::md5::ingest(const void *message, u64 byte_count)
{
	const u8 *msg = reinterpret_cast<const u8*>(message);
	m_message_size += byte_count;
	while (byte_count > 0) {
		u64 bytes_written = 0;
		if (m_chunk_size == 0 && byte_count >= BYTES_PER_CHUNK && is_aligned(msg)) {
			bytes_written = BYTES_PER_CHUNK;
			process_chunk(reinterpret_cast<const u32*>(msg), m_state.u32);
		} else {
			const u64 BYTES_REMAINING = BYTES_PER_CHUNK - m_chunk_size;
			if (byte_count < BYTES_REMAINING) {
				bytes_written = byte_count;
				m_chunk_size += byte_count;
				blit(msg, m_chunk.u8, bytes_written);
			} else {
				bytes_written = BYTES_REMAINING;
				blit(msg, m_chunk.u8, bytes_written);
				process_chunk(m_chunk.u32, m_state.u32);
				m_chunk_size = 0;
			}
		}

		msg += bytes_written;
		byte_count -= bytes_written;
	}
}

cc0::sum::md5::sum cc0::sum::md5::digest( void ) const
{
	sum out;
	memcpy(out, m_state.u8, BYTES_PER_DIGEST);
	process_final_chunks(out.u32());
	if (is_big32()) { // Convert endianess if necessary - digests should always be in the same format no matter what
		for (u32 i = 0; i < BYTES_PER_DIGEST; i += sizeof(u32)) {
			for (u32 j = 0; j < sizeof(u32) >> 1; ++j) {
				const u32 a = i + j;
				const u32 b = i + sizeof(u32) - j - 1;
				const u8 t = out[a];
				out[a] = out[b];
				out[b] = t;
			}
		}
	}
	return out;
}

cc0::sum::md5::operator cc0::sum::md5::sum( void ) const
{
	return digest();
}

//
// sha256
//

/// @brief Initial hash values for the SHA256 algorithm.
static constexpr u32 SHA256_INITIAL_HASH_VALUES[8] = {
	0x6a09e667U,
	0xbb67ae85U,
	0x3c6ef372U,
	0xa54ff53aU,
	0x510e527fU,
	0x9b05688cU,
	0x1f83d9abU,
	0x5be0cd19U
};

u32 cc0::sum::sha256::rrot(u32 l, u32 r) const
{
	return (l >> r) | (l << (32 - r));
}

u32 cc0::sum::sha256::zor(u32 a, u32 b, u32 c) const
{
	return a ^ b ^ c;
}

u32 cc0::sum::sha256::sig(u32 x, u32 s1, u32 s2, u32 s3) const
{
	return zor(rrot(x, s1), rrot(x, s2), (x >> s3));
}

u32 cc0::sum::sha256::SIG(u32 x, u32 s1, u32 s2, u32 s3) const
{
	return zor(rrot(x, s1), rrot(x, s2), rrot(x, s3));
}

u32 cc0::sum::sha256::sig0(u32 x) const
{
	return sig(x, 7, 18, 3);
}

u32 cc0::sum::sha256::sig1(u32 x) const
{
	return sig(x, 17, 19, 10);
}

u32 cc0::sum::sha256::SIG0(u32 x) const
{
	return SIG(x, 2, 13, 22);
}

u32 cc0::sum::sha256::SIG1(u32 x) const
{
	return SIG(x, 6, 11, 25);
}

u32 cc0::sum::sha256::choice(u32 x, u32 y, u32 z) const
{
	return (x & y) ^ ((~x) & z);
}

u32 cc0::sum::sha256::majority(u32 x, u32 y, u32 z) const
{
	return (x & y) ^ (x & z) ^ (y & z);
}

void cc0::sum::sha256::blit(const u8 *src, u8 *dst) const
{
	memcpy(dst, src, BYTES_PER_BLOCK);
}

void cc0::sum::sha256::blit(const u8 *src, u8 *dst, u32 num) const
{
	memcpy(dst, src, num);
	memset(dst + num, 0, BYTES_PER_BLOCK - num);
}

bool cc0::sum::sha256::is_aligned(const void *mem) const
{
	return (reinterpret_cast<uintptr_t>(mem) & (sizeof(u32) - 1)) != 0;
}

void cc0::sum::sha256::create_schedule(const u8 *block, schedule_t &schedule) const
{
	if (is_lil32()) { // NOTE: On little endian machines we need to convert input data to big endian.
		for (u32 i = 0, j = 0; i < 16; i++, j += 4) { // Split data in 32 bit blocks for the 16 first words
			schedule[i] = (u32(block[j]) << 24) | (u32(block[j + 1]) << 16) | (u32(block[j + 2]) << 8) | u32(block[j + 3]);
		}
	} else { // NOTE: We assume machines that are not little endian are big endian (this may not be true for some esoteric architectures).
		memcpy(schedule, block, BYTES_PER_BLOCK);
	}
	for (u32 i = 16; i < WORDS_PER_SCHEDULE; ++i) {
		schedule[i] = sig1(schedule[i-2]) + schedule[i-7] + sig0(schedule[i-15]) + schedule[i-16];
	}
}

void cc0::sum::sha256::process_block(const u8 *block, u32 *X) const
{
	static constexpr schedule_t K = {
		0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
		0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
		0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
		0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
		0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
		0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
		0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
		0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
		0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
		0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
		0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
		0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
		0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
		0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
		0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
		0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
	};

	schedule_t S;
	create_schedule(block, S);

	enum { A, B, C, D, E, F, G, H, REG_COUNT };

	u32 V[REG_COUNT];
	for (u32 i = 0; i < REG_COUNT; ++i) {
		V[i] = X[i];
	}

	for (u32 i = 0; i < WORDS_PER_SCHEDULE; ++i) {
		const u32 T1 = SIG1(V[E]) + choice(V[E], V[F], V[G]) + V[H] + K[i] + S[i];
		const u32 T2 = SIG0(V[A]) + majority(V[A], V[B], V[C]);

		for (u32 j = REG_COUNT - 1; j >= 1; --j) {
			V[j] = V[j-1];
		}
		V[E] += T1;
		V[A] = T1 + T2;
	}

	for (u32 i = 0; i < REG_COUNT; ++i) {
		X[i] += V[i];
	}
}

void cc0::sum::sha256::process_final_blocks(u32 *X) const
{
	u64 byte_count = m_block_size;
	union {
		u32 w32[WORDS_PER_BLOCK];
		u8  w8[BYTES_PER_BLOCK];
	} block;
	//u32 block_data[WORDS_PER_BLOCK];
	//u8 *block = reinterpret_cast<u8*>(block_data);

	// Store size (64 bits) of original message in bits at the end of the message
	u32 padding_size = BYTES_PER_BLOCK - (m_message_size % BYTES_PER_BLOCK);
	if (padding_size < sizeof(u64) + sizeof(u8)) { // Padding must at least fit a 64-bit number to denote message length in bits and one 8-bit number as a terminating 1-bit. If it does not, we add another block to process. Note that since we always work on bytes, not bits, the length of the terminating 1-bit is 8 bits, with a value of 0x80.
		padding_size += BYTES_PER_BLOCK;
	}

	// The message will always be padded in some way. Add a first '1' to the padding.
	memcpy(block.w8, m_block.u8, byte_count);
	block.w8[byte_count] = PADDING_CONST;
	memset(block.w8 + byte_count + 1, 0, BYTES_PER_BLOCK - (byte_count + 1));
	byte_count += padding_size;

	if (byte_count > BYTES_PER_BLOCK) { // Two blocks left to process.
		process_block(block.w8, X);
		memset(block.w8, 0, BYTES_PER_BLOCK - sizeof(u64));
	}

	// One block left to process. Store message size in big endian format.
	const u64 ORIGINAL_MESSAGE_BITSIZE = (m_message_size * CHAR_BIT);
	if (is_lil32()) {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			block.w8[BYTES_PER_BLOCK - 1 - i] = reinterpret_cast<const
			char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	} else {
		for (u32 i = 0; i < sizeof(u64); ++i) {
			block.w8[BYTES_PER_BLOCK - sizeof(u64) + i] = reinterpret_cast<const char*>(&ORIGINAL_MESSAGE_BITSIZE)[i];
		}
	}
	process_block(block.w8, X);
}

cc0::sum::sha256::sha256( void ) : m_message_size(0), m_block_size(0)
{
	for (u32 i = 0; i < WORDS_PER_DIGEST; ++i) {
		m_state.u32[i] = SHA256_INITIAL_HASH_VALUES[i];
	}
}

cc0::sum::sha256::sha256(const char *message) : sha256()
{
	ingest(message);
}

cc0::sum::sha256::sha256(const void *message, u64 byte_count) : sha256()
{
	ingest(message, byte_count);
}

cc0::sum::sha256::~sha256( void )
{
	// Clear sensitive data.
	memset(m_block.u8, 0, sizeof(m_block.u8));
}

cc0::sum::sha256 &cc0::sum::sha256::operator()(const char *message)
{
	ingest(message);
	return *this;
}

cc0::sum::sha256 &cc0::sum::sha256::operator()(const void *message, u64 byte_count)
{
	ingest(message, byte_count);
	return *this;
}

cc0::sum::sha256 cc0::sum::sha256::operator()(const char *message) const
{
	return sha256(*this)(message);
}

cc0::sum::sha256 cc0::sum::sha256::operator()(const void *message, u64 byte_count) const
{
	return sha256(*this)(message, byte_count);
}

void cc0::sum::sha256::ingest(const char *message)
{
	ingest(message, strlen(message));
}

void cc0::sum::sha256::ingest(const void *message, u64 byte_count)
{	
	const u8 *msg = reinterpret_cast<const u8*>(message);
	m_message_size += byte_count;
	while (byte_count > 0) {
		u64 bytes_written = 0;
		if (m_block_size == 0 && byte_count >= BYTES_PER_BLOCK && is_aligned(msg)) {
			bytes_written = BYTES_PER_BLOCK;
			process_block(msg, m_state.u32);
		} else {
			const u64 BYTES_REMAINING = BYTES_PER_BLOCK - m_block_size;
			if (byte_count < BYTES_REMAINING) {
				bytes_written = byte_count;
				m_block_size += byte_count;
				blit(msg, m_block.u8, bytes_written);
			} else {
				bytes_written = BYTES_REMAINING;
				blit(msg, m_block.u8, bytes_written);
				process_block(m_block.u8, m_state.u32);
				m_block_size = 0;
			}
		}

		msg += bytes_written;
		byte_count -= bytes_written;
	}
}

cc0::sum::sha256::sum cc0::sum::sha256::digest( void ) const
{
	sum out;
	memcpy(out, m_state.u8, BYTES_PER_DIGEST);
	process_final_blocks(out.u32());
	if (is_lil32()) { // Convert endianess if necessary - digests should always be in the same format no matter what
		for (u32 i = 0; i < BYTES_PER_DIGEST; i += sizeof(u32)) {
			for (u32 j = 0; j < sizeof(u32) >> 1; ++j) {
				const u32 a = i + j;
				const u32 b = i + sizeof(u32) - j - 1;
				const u8 t = out[a];
				out[a] = out[b];
				out[b] = t;
			}
		}
	}
	return out;
}

cc0::sum::sha256::operator cc0::sum::sha256::sum( void ) const
{
	return digest();
}
