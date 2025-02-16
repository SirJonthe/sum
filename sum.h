/// @file sum.h
/// @brief Contains a minimalist checksum/hashing library.
/// @author github.com/SirJonthe
/// @date 2023
/// @copyright Public domain.
/// @license CC0 1.0

#ifndef CC0_SUM_H_INCLUDED__
#define CC0_SUM_H_INCLUDED__

#include <cstdint>
#include <climits>

namespace cc0
{
	namespace sum
	{
		/// @brief A sequence of bytes generated by a hashing algorithm.
		/// @tparam byte_count The number of bytes in the checksum.
		/// @note Checksums are always stored with the most significant byte at the byte lowest index.
		template < uint64_t byte_count >
		class checksum
		{
		private:
			union {
				uint8_t  u8[byte_count];
				uint32_t u32[byte_count / sizeof(uint32_t)];
			} m_sum;
		
		public:
			/// @brief Default constructor.
			checksum( void );

			/// @brief Copy constructor.
			/// @param NA The object to copy.
			checksum(const checksum&) = default;

			/// @brief Copy assignment.
			/// @param  NA The object to copy.
			/// @return Reference to self.
			checksum &operator=(const checksum&) = default;

			/// @brief Returns the byte data.
			/// @return Pointer to the byte data.
			operator uint8_t*( void );

			/// @brief Returns the byte data.
			/// @return Pointer to the byte data.
			operator const uint8_t*( void ) const;

			/// @brief Returns the word data.
			/// @return Pointer to the word data.
			uint32_t *u32( void );

			/// @brief Returns the word data.
			/// @return Pointer to the word data.
			const uint32_t *u32( void ) const;

			/// Prints the digest into a human-readable hexadeximal format to a string. 
			///
			/// @param out the destination string of the print.
			///
			/// @return the pointer to the location in the sprint at which printing stopped.
			char *sprint_hex(char *out) const;
			/// Prints the digest into a human-readable binary format to a string.
			///
			/// @param out the destination string of the print.
			///
			/// @return the pointer to the location in the sprint at which printing stopped.
			char *sprint_bin(char *out) const;

			/// Compares l < r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator< (const checksum &r) const;
			/// Compares l > r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator> (const checksum &r) const;
			/// Compares l <= r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator<=(const checksum &r) const;
			/// Compares l >= r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator>=(const checksum &r) const;
			/// Compares l == r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator==(const checksum &r) const;
			/// Compares l != r.
			///
			/// @param r the right-hand-side value to compare.
			///
			/// @return the boolean result of the comparison
			bool operator!=(const checksum &r) const;
		};

		/// @brief A CRC32 hash generator.
		class crc32
		{
		private:
			/// @brief A structure containing a pre-computed table to generate CRC32 hashes with.
			class table
			{
			public:
				uint32_t v[256];
			
			public:
				/// @brief Default constructur. Generates the values in the table.
				table( void );

				/// @brief Default copy constructor.
				/// @param  NA The table to copy.
				table(const table&) = default;

				/// @brief Default assignment operator.
				/// @param  NA The table to copy.
				/// @return A reference to self.
				table &operator=(const table&) = default;
			};

		public:
			// typedefs
			typedef cc0::sum::checksum<sizeof(uint32_t)> sum;

		private:
			uint32_t     m_sum;
			static table m_table;

		public:
			/// @brief Default constructor.
			crc32( void );

			/// @brief Constructor. Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			crc32(const char *message);

			/// @brief Constructor. Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			crc32(const void *message, uint64_t byte_count);

			/// @brief Default copy constructor.
			/// @param NA The object to copy.
			crc32(const crc32&) = default;

			/// @brief The default assignment operator.
			/// @param NA THe object to copy.
			/// @return A reference to self.
			crc32 &operator=(const crc32&) = default;

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @return A reference to self.
			crc32 &operator()(const char *message);

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			/// @return A reference to self.
			crc32 &operator()(const void *message, uint64_t byte_count);
			
			/// @brief Copies the object, ingests a message, and updates the internal check sum in the copy.
			/// @param message The message.
			/// @return A copy of the object.
			crc32 operator()(const char *message) const;

			/// @brief Copies the object, ingests a message, and updates the internal check sum in the copy.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			/// @return A copy of the object.
			crc32 operator()(const void *message, uint64_t byte_count) const;

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			void ingest(const char *message);

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			void ingest(const void *message, uint64_t byte_count);

			/// @brief Returns the check sum.
			/// @return The check sum.
			sum digest( void ) const;

			/// @brief Returns the check sum.
			/// @return The check sum.
			operator sum( void ) const;
		};

		/// @brief A FNV1A64 hash generator.
		class fnv1a64
		{
		public:
			// typedefs
			typedef cc0::sum::checksum<sizeof(uint64_t)> sum;

		private:
			uint64_t m_sum;

		public:
			/// @brief Default constructor.
			fnv1a64( void );

			/// @brief Default copy constructor.
			/// @param NA The object to copy.
			fnv1a64(const fnv1a64&) = default;

			/// @brief The default assignment operator.
			/// @param NA THe object to copy.
			/// @return A reference to self.
			fnv1a64 &operator=(const fnv1a64&) = default;

			/// @brief Constructor. Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			fnv1a64(const char *message);

			/// @brief Constructor. Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			fnv1a64(const void *message, uint64_t byte_count);

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @return A reference to self.
			fnv1a64 &operator()(const char *message);

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			/// @return A reference to self.
			fnv1a64 &operator()(const void *message, uint64_t byte_count);
			
			/// @brief Copies the object, ingests a message, and updates the internal check sum in the copy.
			/// @param message The message.
			/// @return A copy of the object.
			fnv1a64 operator()(const char *message) const;

			/// @brief Copies the object, ingests a message, and updates the internal check sum in the copy.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			/// @return A copy of the object.
			fnv1a64 operator()(const void *message, uint64_t byte_count) const;

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			void ingest(const char *message);

			/// @brief Ingests a message and updates the internal check sum.
			/// @param message The message.
			/// @param byte_count The number of bytes in the message.
			void ingest(const void *message, uint64_t byte_count);

			/// @brief Returns the check sum.
			/// @return The check sum.
			sum digest( void ) const;

			/// @brief Returns the check sum.
			/// @return The check sum.
			operator sum( void ) const;
		};

		/// Processes messages of any length into a relatively unique identifyer with a length of 16 bytes. Functions by ingesting any number of messages via the 'ingest' function (alternatively via constructors and () operators) and finally outputting an MD5 sum via the 'digest' function. New messages can be appended even after a digest has been generated.
		///
		/// @note MD5 can only process a maximum of 2^64-1 bytes (be careful as there is no guard for overflow). This implementation only processes input messages in whole bytes, and can not be used to process messages composed of individual bits.
		/// @note MD5 is considered insecure for cryptographic purposes.
		class md5
		{
		private:
			// constants
			static constexpr uint32_t BYTES_PER_DIGEST = 16;
			static constexpr uint32_t WORDS_PER_DIGEST = BYTES_PER_DIGEST / sizeof(uint32_t);
			static constexpr uint32_t BYTES_PER_CHUNK  = 512 / CHAR_BIT;
			static constexpr uint32_t WORDS_PER_CHUNK  = BYTES_PER_CHUNK / sizeof(uint32_t);

		public:
			// typedefs
			typedef cc0::sum::checksum<BYTES_PER_DIGEST> sum;

		private:
			union {
				uint32_t u32[WORDS_PER_DIGEST];
				uint8_t  u8[BYTES_PER_DIGEST];
			} m_state;
			union {
				uint32_t u32[WORDS_PER_CHUNK];
				uint8_t  u8[BYTES_PER_CHUNK];
			} m_chunk;
			uint64_t m_message_size;
			uint32_t m_chunk_size;

		private:
			/// Bit-block transfer of 64 bytes from 'src' to 'dst'.
			///
			/// @param src the source to write.
			/// @param dst the destination to write to.
			static void blit(const uint8_t *src, uint8_t *dst);
			/// Bit-block transfer of 'num' bytes from 'src' to 'dst'. Fills remaining 64-'num' bytes in 'dst' with zero-value.
			///
			/// @param src the source to write.
			/// @param dst the destination to write to.
			/// @param num the number of bytes to write.
			static void blit(const uint8_t *src, uint8_t *dst, uint32_t num);
			/// Checks if the memory is aligned to a 4-byte boundry.
			///
			/// @param mem the memory location to check for alignment.
			///
			/// @return boolean indicating true if the memory is 4-byte aligned, and false elsewise.
			static bool is_aligned(const void *mem);
			/// Returns a the left rotation of bits in 'x' by amount 'c'. Bits shifted out are shifted back in from the right.
			///
			/// @param x the data to rotate.
			/// @param c the amount steps to rotate the data.
			///
			/// @return the rotated data.
			static uint32_t leftrotate(uint32_t x, uint32_t c);
			/// Processes a single message data block and transforms the digest values in 'X'.
			///
			/// @param M pointer to the message block.
			/// @param X pointer to the destination block.
			void process_chunk(const uint32_t *M, uint32_t *X) const;
			/// Processes the remaining data in the block buffer so that a digest can be returned.
			///
			/// @param X the block to do a final transform on.
			void process_final_chunks(uint32_t *X) const;

		public:
			/// Default constructor. Sets up the initial internal state.
			md5( void );
			/// Ingest an initial message. Length is inferred from zero-terminator.
			///
			/// @param message pointer to a message to ingest.
			md5(const char *message);
			/// Ingest an initial message. Explicit length.
			///
			/// @param message pointer to a message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			md5(const void *message, uint64_t byte_count);
			/// Clear out sensitive data.
			~md5( void );

			/// Default copy constructor.
			md5(const md5&) = default;
			/// Default assingment operator.
			md5 &operator=(const md5&) = default;

			/// Ingest a message. Length is inferred from zero-terminator.
			///
			/// @param message the message to ingest.
			///
			/// @return a reference to the modified data (self).
			md5 &operator()(const char *message);
			/// Ingest a message. Explicit length.
			///
			/// @param message the message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			///
			/// @return a reference to the modified data (self).
			md5 &operator()(const void *message, uint64_t byte_count);

			/// Returns a copy of current state with ingested message. Length is inferred from zero-terminator.
			///
			/// @param message the message to ingest.
			///
			/// @return a modified md5 incorporating the ingestion.
			md5 operator()(const char *message) const;
			/// Returns a copy of current state with ingested message. Explicit length.
			///
			/// @param message the message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			///
			/// @return a modified md5 incorporating the ingestion.
			md5 operator()(const void *message, uint64_t byte_count) const;

			/// Ingest a message. Length is inferred from zero-terminator.
			///
			/// @param message the message to ingest.
			void ingest(const char *message);
			/// Ingest a message. Explicit length.
			///
			/// @param message the message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			void ingest(const void *message, uint64_t byte_count);

			/// Returns the digest of all ingested messages.
			///
			/// @return the digest.
			sum digest( void ) const;
			/// Implicitly converts state into digest of all ingested messages.
			///
			/// @return the digest.
			operator sum( void ) const;
		};


		/// @brief Processes messages of any length into a very unique identifyer with a length of 32 bytes. Functions by ingesting any number of messages via the 'ingest' function (alternatively via constructors and () operators) and finally outputting an SHA256 sum via the 'digest' function. New messages can be appended even after a digest has been generated.
		class sha256
		{
		private:
			// constants
			static constexpr uint32_t BITS_PER_BYTE      = CHAR_BIT;
			static constexpr uint32_t BITS_PER_DIGEST    = 256;
			static constexpr uint32_t BITS_PER_BLOCK     = 512;
			static constexpr uint32_t BYTES_PER_DIGEST   = BITS_PER_DIGEST / BITS_PER_BYTE;
			static constexpr uint32_t WORDS_PER_DIGEST   = BYTES_PER_DIGEST / sizeof(uint32_t);
			static constexpr uint32_t BYTES_PER_BLOCK    = BITS_PER_BLOCK / BITS_PER_BYTE;
			static constexpr uint32_t WORDS_PER_BLOCK    = BYTES_PER_BLOCK / sizeof(uint32_t);
			static constexpr uint32_t WORDS_PER_SCHEDULE = 64;
			static constexpr uint8_t  PADDING_CONST      = 1 << 7;

			// typedefs
			typedef uint32_t schedule_t[WORDS_PER_SCHEDULE];
		
		public:
			// typedefs
			typedef cc0::sum::checksum<BYTES_PER_DIGEST> sum;

		private:
			union {
				uint32_t u32[WORDS_PER_DIGEST];
				uint8_t  u8[BYTES_PER_DIGEST];
			} m_state;
			union {
				uint32_t u32[WORDS_PER_BLOCK];
				uint8_t  u8[BYTES_PER_BLOCK];
			} m_block;
			uint64_t m_message_size;
			uint32_t m_block_size;

		private:
			/// Returns a the right rotation of bits in 'l' by amount 'r'. Bits shifted out are shifted back in from the left.
			///
			/// @param l the value to rotate.
			/// @param r the number of iterations to rotate the value.
			///
			/// @return the rotated bits.
			uint32_t rrot(uint32_t l, uint32_t r) const;
			/// Exclusive-or between three values.
			///
			/// @param a a value.
			/// @param b a value.
			/// @param c a value.
			///
			/// @return the zored value.
			uint32_t zor(uint32_t a, uint32_t b, uint32_t c) const;
			/// Generic main lower case sigma function.
			///
			/// @param x a value.
			/// @param s1 the number of right rotations for the first step.
			/// @param s2 the number of right rotations for the second step.
			/// @param s3 the number of shifts for the third step.
			///
			/// @return the lower case sigma.
			uint32_t sig(uint32_t x, uint32_t s1, uint32_t s2, uint32_t s3) const;
			/// Generic main upper case sigma function.
			///
			/// @param x a value.
			/// @param s1 the number of right rotations for the first step.
			/// @param s2 the number of right rotations for the second step.
			/// @param s3 the number of right rotations for the third step.
			///
			/// @return the upper case sigma.
			uint32_t SIG(uint32_t x, uint32_t s1, uint32_t s2, uint32_t s3) const;
			/// Lower case sigma zero. Performs a transformation on one input value.
			///
			/// @param x the value.
			///
			/// @return the lower case sigma zero.
			uint32_t sig0(uint32_t x) const;
			/// Lower case sigma one. Performs a transformation on one input value.
			///
			/// @param x a value.
			///
			/// @return the lower case sigma one.
			uint32_t sig1(uint32_t x) const;
			/// Upper case sigma zero. Performs a transformation on one input value.
			///
			/// @param x a value.
			///
			/// @return the upper case sigma zero.
			uint32_t SIG0(uint32_t x) const;
			/// Upper case sigma one. Performs a transformation on one input value.
			///
			/// @param x a value.
			///
			/// @return the upper case sigma one.
			uint32_t SIG1(uint32_t x) const;
			/// Returns a bit array where the result is picked between 'y' and 'z' using 'x' as boolean selector. 1 picks from 'y', 0 picks from 'z'.
			///
			/// @param x a value.
			/// @param y a value.
			/// @param z a value.
			///
			/// @return the choise bits.
			uint32_t choice(uint32_t x, uint32_t y, uint32_t z) const;
			/// Returns a bit array where the result is equal to the majority bit value for a given bit position between 'x', 'y', and 'z'.
			///
			/// @param x a value.
			/// @param y a value.
			/// @param z a value.
			///
			/// @return the majority bits.
			uint32_t majority(uint32_t x, uint32_t y, uint32_t z) const;
			/// Bit-block transfer of 64 bytes from 'src' to 'dst'.
			///
			/// @param src the source to write.
			/// @param dst the destination to write to.
			void blit(const uint8_t *src, uint8_t *dst) const;
			/// Bit-block transfer of 'num' bytes from 'src' to 'dst'. Fills remaining 64-'num' bytes in 'dst' with zero-value.
			///
			/// @param src the source to write.
			/// @param dst the destination to write to.
			/// @param num the number of bytes to write.
			void blit(const uint8_t *src, uint8_t *dst, uint32_t num) const;
			/// Checks if the memory is aligned to a 4-byte boundary.
			///
			/// @param mem the memory location to check for alignment.
			///
			/// @return boolean indicating true if the memory is 4-byte aligned, and false elsewise.
			bool is_aligned(const void *mem) const;
			/// Fills a message schedule with the contents from 'block' and calculates the remaining 48 words in the schedule.
			///
			/// @param block the pointer to the block from which to create a schedule from.
			/// @param schedule the destination of the output schedule.
			void create_schedule(const uint8_t *block, schedule_t &schedule) const;
			/// Processes a single message data block and transforms the digest values in 'X'.
			///
			/// @param block the pointer to the block to process.
			/// @param X the pointer to the digest to store the result of the process.
			void process_block(const uint8_t *block, uint32_t *X) const;
			/// Processes the remaining data in the block buffer so that a digest can be returned.
			///
			/// @param X the pointer to the digest to store the result of the process.
			void process_final_blocks(uint32_t *X) const;

		public:
			/// Initialize digest to proper seed values.
			sha256( void );
			/// Ingest an initial message. Length is inferred from zero-terminator.
			///
			/// @param message pointer to a message to ingest.
			sha256(const char *message);
			/// Ingest an initial message. Explicit length.
			///
			/// @param message pointer to a message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			sha256(const void *message, uint64_t byte_count);
			/// Clear out sensitive data.
			~sha256( void );

			/// Default copy constructor.
			sha256(const sha256&) = default;
			/// Default assignment operator.
			sha256 &operator=(const sha256&) = default;

			/// Ingest a message. Length is inferred from zero-terminator.
			///
			/// @param message pointer to a message to ingest.
			///
			/// @return a reference to the modified data (self).
			sha256 &operator()(const char *message);
			/// Ingest a message. Explicit length.
			///
			/// @param message pointer to a message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			///
			/// @return a reference to the modified data (self).
			sha256 &operator()(const void *message, uint64_t byte_count);

			/// Returns a copy of current state with ingested message. Length is inferred from zero-terminator.
			///
			/// @param message pointer to the message to ingest.
			///
			/// @return a modified sha256 incorporating the ingestion.
			sha256 operator()(const char *message) const;
			/// Returns a copy of current state with ingested message. Explicit length.
			///
			/// @param message pointer to the message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			///
			/// @return a modified sha256 incorporating the ingestion.
			sha256 operator()(const void *message, uint64_t byte_count) const;

			/// Ingest a message. Length is inferred from zero-terminator.
			///
			/// @param message pointer to a message to ingest.
			void ingest(const char *message);
			/// Ingest a message. Explicit length.
			///
			/// @param message pointer to a message to ingest.
			/// @param byte_count the number of bytes in the message to ingest.
			void ingest(const void *message, uint64_t byte_count);

			/// Returns the digest of all ingested messages.
			///
			/// @return the digest.
			sum digest( void ) const;
			/// Implicitly converts state into digest of all ingested messages.
			///
			/// @return the digest.
			operator sum( void ) const;
		};
	}
}

//
// checksum
//

template < uint64_t byte_count >
cc0::sum::checksum<byte_count>::checksum( void )
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		m_sum.u8[i] = 0;
	}
}

template < uint64_t byte_count >
cc0::sum::checksum<byte_count>::operator uint8_t*( void )
{
	return m_sum.u8;
}

template < uint64_t byte_count >
cc0::sum::checksum<byte_count>::operator const uint8_t*( void ) const
{
	return m_sum.u8;
}

template < uint64_t byte_count >
uint32_t *cc0::sum::checksum<byte_count>::u32( void )
{
	return m_sum.u32;
}

template < uint64_t byte_count >
const uint32_t *cc0::sum::checksum<byte_count>::u32( void ) const
{
	return m_sum.u32;
}

template < uint64_t byte_count >
char *cc0::sum::checksum<byte_count>::sprint_hex(char *out) const
{
	static constexpr char DIGITS[] = "0123456789abcdef";
	for (uint64_t i = 0; i < sizeof(m_sum); ++i, out += 2) {
		uint8_t b = m_sum.u8[i];
		out[0] = DIGITS[b >> 4];
		out[1] = DIGITS[b & 15];
	}
	return out;
}

template < uint64_t byte_count >
char *cc0::sum::checksum<byte_count>::sprint_bin(char *out) const
{
	for (uint64_t byte = 0; byte < sizeof(m_sum); ++byte) {
		for (uint64_t bit = 0; bit < CHAR_BIT; ++bit, ++out) {
			out[0] = (m_sum.u8[byte]  & (1 << (CHAR_BIT - 1 - bit))) ? '1' : '0';
		}
	}
	return out;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator< (const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] >= r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator> (const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] <= r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator<=(const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] > r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator>=(const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] < r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator==(const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] != r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

template < uint64_t byte_count >
bool cc0::sum::checksum<byte_count>::operator!=(const cc0::sum::checksum<byte_count> &r) const
{
	for (uint64_t i = 0; i < byte_count; ++i) {
		if (m_sum.u8[i] == r.m_sum.u8[i]) {
			return false;
		}
	}
	return true;
}

#endif
